%%% @doc Fetch statistics off of the QuakeLive homepage
-module(ql_fetch).

-export([player_matches/2,
         match/1]).

player_matches(Player, Weeks) ->
    player_matches(Player, Weeks, []).

player_matches(_Player, [], Matches) ->
    {ok, Matches};
player_matches(Player, [Week | NextWeek], Acc) ->
    case request_player_matches(Player, Week) of
        {ok, Body} ->
            Matches = parse_matches(Body),
            lager:debug("Fetched - Player: ~s, Week: ~p: ~B",
                        [Player, Week, length(Matches)]),
            timer:sleep(3000), % Wait a bit before fetching next week
            player_matches(Player, NextWeek, Matches ++ Acc);
        {error, Reason} ->
            {error, Reason}
    end.

request_player_matches(Player, Week) when is_list(Player) ->
    URL = week_matches_url(Player, Week),
    case request(URL) of
        {ok, Body} ->
            {ok, parse_matches(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

match(Match) ->
    URL = match_url(Match),
    case request(URL) of
        {ok, Body} ->
            {ok, jsx:json_to_term(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

parse_matches(Body) ->
    {ok, REC} =
        re:compile("<div class=\"areaMapC\" " ++
                       "id=\"duel_([a-z0-9-]{36})_1\">"),
    case re:run(Body, REC, [global, {capture, all, binary}]) of
        {match, Ms} ->
            [M || [_, M] <- Ms];
        nomatch ->
            []
    end.

match_url(Match) when is_binary(Match) ->
    string:join(
      ["http://www.quakelive.com/stats/matchdetails",
       binary_to_list(Match)], "/").

week_matches_url(Player, {YYYY, MM, DD}) ->
    Base = "http://www.quakelive.com/profile/matches_by_week",
    WeekStr = io_lib:format("~B-~2..0B-~2..0B", [YYYY, MM, DD]),
    string:join([Base, Player, lists:flatten(WeekStr)], "/").

request(URL) ->
    From = now(),
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{_HTTPVer, 200, _RPhrase}, _Headers, Body}} ->
            To = now(),
            case overload(From, To) of
                true ->
                    ok = lager:info("Overload at QL Site"),
                    qlg_overload:overload(),
                    jobs_sampler:tell_sampler(response_time, overload);
                false ->
                    ok
            end,
            {ok, Body};
        {ok, {{_HttpVer, 502, _RPhrase}, _Headers, _Body} = SC} ->
            lager:info("Got 502 Bad Gateway"),
            qlg_overload:timeout(),
            {error, {status_code, SC}};
        {ok, Otherwise} ->
            ok = lager:info("Timeouts, assuming overload"),
            qlg_overload:overload(),
            jobs_sampler:tell_sampler(response_time, overload),
            {error, {status_code, Otherwise}};
        {error, Reason} ->
            lager:info("HTTP request error: ~p", [Reason]),
            qlg_overload:timeout(),
            {error, Reason}
    end.

overload(From, To) ->
    calc_interval(From, To) > 4000.

calc_interval({FMega, FSec, FMicro} = F, {TMega, TSec, TMicro} = T)
  when F < T ->
    FMs = (FMega * 1000000 + FSec) * 1000 + (FMicro / 1000),
    TMs = (TMega * 1000000 + TSec) * 1000 + (TMicro / 1000),
    TMs - FMs.
