%%% @doc Fetch statistics off of the QuakeLive homepage
-module(ql_fetch).

-export([player_matches/1,
         match/1]).

player_matches(Player) ->
    URL = week_matches_url(Player, calendar:universal_time()),
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
    {match, Matches} = re:run(Body, REC, [global, {capture, all, binary}]),
    [M || [_, M] <- Matches].

match_url(Match) when is_binary(Match) ->
    string:join(
      ["http://www.quakelive.com/stats/matchdetails",
       binary_to_list(Match)], "/").

week_matches_url(Player, {{YYYY, MM, DD}, _}) ->
    Base = "http://www.quakelive.com/profile/matches_by_week",
    WeekStr = io_lib:format("~B-~2..0B-~2..0B", [YYYY, MM, DD]),
    string:join([Base, Player, lists:flatten(WeekStr)], "/").

request(URL) ->
    From = now(),
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{_HTTPVer, 200, _RPhrase}, _Headers, Body}} ->
            To = now(),
            ql_response_time:report(From, To),
            {ok, Body};
        {ok, Otherwise} ->
            {error, {status_code, Otherwise}};
        {error, Reason} ->
            {error, Reason}
    end.



