-module(qlg_rank).

-export([rank/1, rank/2, rank/3,
         init_players/0,
         load_keep/0,
         load_all/0,
         unload_all/0,
         predict/2,
         rate_game/2,
         expected_score/3, expected_score/4,
         write_csv/2,
         matrix/3,
         ps/0,
         load_tournaments/1]).
-export([loader_looper/0]).

-define(CHUNK_SIZE, 15000).

ps() ->
    [P || P <- ["Raziel2p",
                "danielvegas",
                "cozmic",
                "CrazyAl90",
                "beffy",
                "KillaloT",
                "pb",
                "ollir",
                "Nikkezhu",
                "bezoeker",
                "kristus",
                "QuadmanSWE",
                "Relativizor",
                "Cooller"],
          is_player(iolist_to_binary(P))].

is_player(P) ->
    case ets:match(qlg_players, {'$1', P}) of
        [] ->
            false;
        [_ | _] ->
            true
    end.

unload_all() ->
    ets:delete(qlg_matches).

load_keep() ->
    spawn(fun() ->
                  load_all(),
                  loader_looper()
          end).

loader_looper() ->
    receive
        stop ->
            ok
    after 2000 ->
            ?MODULE:loader_looper()
    end.
        
load_all() ->
    {ok, Ts} = qlg_pgsql_srv:all_tournaments(),
    load_tournaments(Ts),
    {ok, Players} = qlg_pgsql_srv:all_players(),
    load_players(Players),
    {ok, ets:info(qlg_matches, size), ets:info(qlg_players, size)}.

load_players(Players) ->
    ets:new(qlg_players, [named_table, public, {read_concurrency, true},
                          set]),
    [ets:insert(qlg_players, {Id, Name}) || {Id, Name} <- Players],
    ok.

load_tournaments(Ts) ->
    ets:new(qlg_matches,
            [named_table, public, {read_concurrency, true},
             duplicate_bag]),
    Counted = lists:zip(lists:seq(1, length(Ts)), Ts),
    [load_tournament(K, T) || {K, T} <- Counted].

%% @doc Load tournaments from disk
%% This populates the qlg_matches table with a number of tournaments
%% from the disk.
load_tournament(Idx, T) ->
    Matches = qlg_pgsql_srv:tournament_matches(T),
    [begin
         ets:insert(qlg_matches, {{Idx, {Winner, w}}, Loser}),
         ets:insert(qlg_matches, {{Idx, {Loser, l}}, Winner})
     end || {Winner, Loser} <- Matches].

init_players() ->
    init_players(1). %% First tournament

init_players(Idx) ->
    Players = ets:match(qlg_matches, {{Idx, {'$1', '_'}}, '_'}),
    S = lists:usort([P || [P] <- Players]),
    {ok, S, length(S)}.

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

predict(DB, Idx) ->
    Matches = ets:match(qlg_matches, {{Idx, {'$1', w}}, '$2'}),
    Predicted = predict_matches(DB, Matches),
    lists:sum(Predicted) / length(Predicted).

rate_game(Y, E) ->
    -(Y * math:log10(E) + (1 - Y)*math:log10(1-E)).

q() ->
    math:log(10) / 400.

expected_g(RD) ->
    1 / (math:sqrt(1 + 3*q()*q()*RD*RD/(math:pi()*math:pi()))).

commatize([]) -> [];
commatize([E]) -> [E];
commatize([A| Rest]) -> [A, $,, commatize(Rest)].

matrix(Db, Players, Fname) ->
    Bins = [iolist_to_binary(P) || P <- Players],
    Matrix = [[P1, commatize([io_lib:format("~6.2. f", [expected_score(Db, P1, Opponent)]) || Opponent <- Bins]), $\n] || P1 <- Bins],
    Header = [$ , $, , commatize(Players), $\n],
    Data = [Header | [commatize(L) || L <- Matrix]],
    file:write_file(Fname, Data).

expected_score(Db, P1, P2) ->
    [[Id1]] = ets:match(qlg_players, {'$1', P1}),
    [[Id2]] = ets:match(qlg_players, {'$1', P2}),
    {R1, RD1} = player_rating(Db, Id1),
    {R2, RD2} = player_rating(Db, Id2),
    expected_score(R1, RD1, R2, RD2).

expected_score(WR, WRD, LR, LRD) ->
    GVal = expected_g(math:sqrt(WRD*WRD + LRD*LRD)),
    1 / (1 + math:pow(10, -GVal * (WR - LR) / 400)).

predict_matches(_Db, []) -> [];
predict_matches(Db, [[Winner, Loser] | Next]) ->
    try
        {WR, WRD} = player_rating(Db, Winner),
        {LR, LRD} = player_rating(Db, Loser),
        E = expected_score(WR, WRD, LR, LRD),
        [rate_game(1, E) | predict_matches(Db, Next)]
    catch
        _:_ ->
            predict_matches(Db, Next)
    end.

player_rating(DB, P) ->
    {R, RD, _} = dict:fetch(P, DB),
    {R, RD}.

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

clamp(Lo, X, _ ) when X < Lo -> Lo;
clamp(_,  X, Hi) when X > Hi -> Hi;
clamp(_,  X, _ ) -> X.

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

