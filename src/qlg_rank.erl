-module(qlg_rank).

-export([rank/1, rank/2, rank/3,
         read/2,
         predict/2,
         rate_game/2,
         write_csv/2,
         matrix/1,
         write_player_csv/2,
         write_matrix/2,
         process_tourney/3]).

%% Ranking
%% ----------------------------------------------------------------------

init_players(Idx) ->
    Players = ets:match(qlg_matches, {{Idx, {'$1', '_'}}, '_'}),
    S = lists:usort([P || [P] <- Players]),
    {ok, S, length(S)}.

player_ranking(DB, P, _Idx, Conf) ->
    case dict:find(P, DB) of
        error ->
            {RD, Sigma, _} = glicko2:read_config(Conf),
            R = 1500,
            %Played = matches_played(Idx, P, w) + matches_played(Idx, P, l),
            %IRD = RD / math:sqrt(Played) + 25,
            {P, R, RD, Sigma};
        {ok, {R, RD, Sigma}} ->
            {P, R, RD, Sigma}
    end.

clamp(Lo, X, _ ) when X < Lo -> Lo;
clamp(_,  X, Hi) when X > Hi -> Hi;
clamp(_,  X, _ ) -> X.


rank_player(Db, Player, Idx, Conf) ->
    {Player, R, RD, Sigma} = player_ranking(Db, Player, Idx, Conf),
        Wins = [begin
                {_P, LR, LRD, _Sigma} = player_ranking(Db, Opp, Idx, Conf),
                {LR, LRD, 1}
            end || {_, Opp} <- ets:lookup(qlg_matches, {Idx, {Player, w}})],
    Losses = [begin
                  {_P, WR, WRD, _Sigma} = player_ranking(Db, Opp, Idx, Conf),
                  {WR, WRD, 0}
              end || {_, Opp} <- ets:lookup(qlg_matches, {Idx, {Player, l}})],
    case Wins ++ Losses of
        [] ->
            RD1 = glicko2:phi_star(RD, Sigma),
            {Player, {R, RD1, Sigma}};
        Opponents ->
            {R1, RD1, Sigma1} =
                glicko2:rate(R, RD, Sigma, Opponents, Conf),
            {Player, {clamp(0.0, R1, 3000.0),
                      clamp(0.0, RD1, 400.0),
                      clamp(0.0, Sigma1, 0.1)}}
    end.

rank(Idxs) ->
    rank(Idxs, glicko2:configuration(350, 0.06, 0.5)).

rank(Idxs, Conf) ->
    PDB = dict:new(),
    rank(PDB, Idxs, Conf).

rank(DB, [I | Next], Conf) ->
    OldPlayers = dict:fetch_keys(DB),
    {ok, Players, _L} = init_players(I),
    NewDB = rank(DB, lists:usort(OldPlayers ++ Players), I, Conf),
    rank(dict:from_list(NewDB), Next, Conf);
rank(DB, [], _Conf) ->
    {ok, DB}.

rank(DB, [Player | Next], Idx, Conf) ->
    Rank = rank_player(DB, Player, Idx, Conf),
    [Rank | rank(DB, Next, Idx, Conf)];
rank(_DB, [], _Idx, _) -> [].

%% Prediction
%% ----------------------------------------------------------------------

predict(DB, Idx) ->
    Matches = ets:match(qlg_matches, {{Idx, {'$1', w}}, '$2'}),
    Predicted = predict_matches(DB, Matches),
    lists:sum(Predicted) / length(Predicted).

q() ->
    math:log(10) / 400.

rate_game(Y, E) ->
    -(Y * math:log10(E) + (1 - Y)*math:log10(1-E)).

expected_g(RD) ->
    1 / (math:sqrt(1 + 3*q()*q()*RD*RD/(math:pi()*math:pi()))).

commatize([]) -> [];
commatize([E]) -> [E];
commatize([A| Rest]) -> [A, $,, commatize(Rest)].

process_tourney(Source, DestPrefix, Db) ->
    {Players, _Groups} = read(Source, Db),
    write_matrix(DestPrefix ++ "_matrix.csv", Players),
    write_player_csv(DestPrefix ++ "_rankings.csv", Players),
    ok.

lookup_players(Gps, Db) ->
    Players = lists:concat([Ps || {group, _, Ps} <- Gps]),
    [lookup_rating(P, Db) || P <- Players].

lookup_rating(P, Db) ->
    case ets:match(qlg_players, {'$1', iolist_to_binary(P)}) of
        [[Id]] ->
            {R, RD, Sigma} = dict:fetch(Id, Db),
            {player, P, R, RD, Sigma};
        [] ->
            {player, P, 1500.0, 350.0, 0.06}
    end.

expected_score({player, _A, RA, RDA, _}, {player, _B, RB, RDB, _}) ->
    expected_score(RA, RDA, RB, RDB).

expected_score(WR, WRD, LR, LRD) ->
    GVal = expected_g(math:sqrt(WRD*WRD + LRD*LRD)),
    1 / (1 + math:pow(10, -GVal * (WR - LR) / 400)).

player_rating(P, DB) ->
    {R, RD, _} = dict:fetch(P, DB),
    {R, RD}.

predict_matches(_Db, []) -> [];
predict_matches(Db, [[Winner, Loser] | Next]) ->
    try
        {WR, WRD} = player_rating(Winner, Db),
        {LR, LRD} = player_rating(Loser, Db),
        E = expected_score(WR, WRD, LR, LRD),
        [rate_game(1, E) | predict_matches(Db, Next)]
    catch
        _:_ ->
            predict_matches(Db, Next)
    end.

read(Fn, Db) ->
    {ok, [_Players, Groups]} = file:consult(Fn),
    {lookup_players(Groups, Db),
     Groups}.


%% Output formatting
%% ----------------------------------------------------------------------
write_matrix(Fn, Players) ->
    M = matrix(Players),
    file:write_file(Fn, M).

write_player_csv(Fn, Players) ->
    file:write_file(Fn,
                    ["Player,R,RD,Sigma\n" |
                     [[io_lib:format("~s,~f,~f,~f\n", [P, R, RD, Sigma])
                       || {player, P, R, RD, Sigma} <- Players]]]).

write_csv(Fname, Db) ->
    Data =
        dict:fold(fun(Id, Ranking, Acc) ->
                          [format_player(Id, Ranking), $\n | Acc]
                  end,
                  [], Db),
    file:write_file(Fname, [["Player,R,RD,Sigma", $\n] | Data]).


format_player(Id, {R, Rd, S}) ->
    [{Id, Name}] = ets:lookup(qlg_players, Id),
    [Name, $,, float_to_list(R), $,, float_to_list(Rd), $,, float_to_list(S)].

name({player, N, _, _, _}) -> N.

matrix(Players) ->
    Matrix = [[name(P), $,, commatize([io_lib:format("~6.2. f", [expected_score(P, O)])
                             || O <- Players]), $\n]
              || P <- Players],
    Header = ["Name,", commatize([name(P) || P <- Players]), $\n],
    [Header | Matrix].

