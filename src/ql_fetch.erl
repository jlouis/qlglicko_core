%%% @doc Fetch statistics off of the QuakeLive homepage
-module(ql_fetch).

-export([player_matches/2,
	alive_check/1,
         match/1]).

player_matches(Player, Weeks) ->
    player_matches(Player, Weeks, []).

player_matches(_Player, [], Matches) ->
    {ok, Matches};
player_matches(Player, [Week | NextWeek], Acc) ->
    case request_player_matches(Player, Week) of
        {ok, Matches} ->
            lager:debug("Fetched - Player: ~s, Week: ~p: ~B",
                        [Player, Week, length(Matches)]),
            timer:sleep(1000), % Wait a bit before fetching next week
            player_matches(Player, NextWeek, Matches ++ Acc);
        {error, Reason} ->
            {error, Reason}
    end.

alive_check(Name) ->
    URL = alive_url(Name),
    case request(URL) of
        {ok, Body} ->
          {ok, RE} = re:compile("Unknown Player"),
          case re:run(Body, RE, [global, {capture, all, binary}]) of
            nomatch -> alive;
            {match, _} -> account_closed
          end;
        {error, Reason} ->
          {error, Reason}
    end.

request_player_matches(Player, Week) when is_list(Player) ->
    URL = week_matches_url(Player, Week),
    case request(URL) of
        {ok, Body} ->
            parse_matches(Body);
        {error, Reason} ->
            {error, Reason}
    end.

match(Match) ->
    URL = match_url(Match),
    request(URL).

parse_matches(Body) ->
    {ok, REC} =
        re:compile("<div class=\"areaMapC\" " ++
                       "id=\"duel_([a-z0-9-]{36})_1\">"),
    case re:run(Body, REC, [global, {capture, all, binary}]) of
        {match, Ms} ->
            {ok, [M || [_, M] <- Ms]};
        nomatch -> {ok, []}
    end.

match_url(Match) when is_binary(Match) ->
    iolist_to_binary(["http://www.quakelive.com/stats/matchdetails", Match, "/"]).

week_matches_url(Player, {YYYY, MM, DD}) -> 
    Base = "http://www.quakelive.com/profile/matches_by_week",
    WeekStr = io_lib:format("~B-~2..0B-~2..0B", [YYYY, MM, DD]),
    iolist_to_binary([Base, Player, lists:flatten(WeekStr), "/"]).

alive_url(Name) ->
    iolist_to_binary(["http://www.quakelive.com/profile/summary", Name, "/"]).

request(URL) ->
    Options = [{pool, default}],

    From = erlang:monotonic_time(),
    case hackney:request(get, URL, [], <<>>, Options) of
        {ok, 200, _, ClientRef} ->
          {ok, Body} = hackney:body(ClientRef),
          To = erlang:monotonic_time(),
          case erlang:convert_time_unit(To - From, native, milli_seconds) of
              L when L > 6000  ->
                  ok = lager:info("Overload at QL Site"),
                  qlg_overload:overload(),
                  ok;
              _Otherwise ->
              ok
          end,
          hackney:close(ClientRef),
          {ok, Body};
        {ok, 502, _, ClientRef} ->
            lager:info("Got 502 Bad Gateway"),
            qlg_overload:timeout(),
            hackney:close(ClientRef),
            {error, {status_code, 502}};
        {ok, 504, _, ClientRef} ->
            lager:info("Got 504 Gateway timeout"),
            qlg_overload:timeout(),
            hackney:close(ClientRef),
            {error, {status_code, 504}};
        {ok, Otherwise, _, ClientRef} ->
            ok = lager:info("Timeouts, assuming overload"),
            qlg_overload:overload(),
            hackney:close(ClientRef),
            {error, {status_code, Otherwise}};
        {error, Reason} ->
            lager:info("HTTP request error: ~p", [Reason]),
            qlg_overload:timeout(),
            {error, Reason}
    end.
