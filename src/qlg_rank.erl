-module(qlg_rank).

-export([rank/3,
         expected_score/4,
	run/1, profile/0,
         read/2,
         predict/2,
         rate_game/2,
         matrix/1,
         write_player_csv/2,
         write_matrix/2,
         process_tourney/3]).

-export([load_keep/0, unload_all/0, loader_looper/0]).

%% Loading
%% ----------------------------------------------------------------------

unload_all() ->
    ets:delete(qlg_matches).

load_keep() ->
    spawn(fun() ->
                  {ok, Matches, Players} = load_all(),
                  lager:notice("Loaded ~B matches and ~B players", [Matches, Players]),
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
         W = {Winner, Map},
         L = {Loser, Map},
         ets:insert(qlg_matches, {{Idx, {W, w}}, L}),
         ets:insert(qlg_matches, {{Idx, {L, l}}, W})
     end || {Winner, Loser, Map} <- Matches].

load_players(Players) ->
    ets:new(qlg_players, [named_table, public, {read_concurrency, true},
                          set]),
    [ets:insert(qlg_players, {Id, Name}) || {Id, Name} <- Players],
    ok.


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

profile() ->
    eprof:profile(fun() -> qlg_rank:run(lists:seq(1,10)) end),
    eprof:stop_profiling(),
    eprof:log("eprof.run"),
    eprof:analyze().

run(Idxs) ->
    Fd = setup_file("rankings.csv"),   
    {ok, Db} = rank(Idxs, glicko2:configuration(397, 0.07, 0.3), Fd),
    file:close(Fd),
    {ok, Db}.

rank(Idxs, Conf, Fd) ->
    PDB = dict:new(),
    rank_tourney(PDB, Idxs, Conf, Fd).

rank_tourney(DB, [I | Next], Conf, Fd) ->
    OldPlayers = dict:fetch_keys(DB),
    {ok, Players, _L} = init_players(I),
    NewDB = rank(DB, lists:usort(OldPlayers ++ Players), I, Conf),
    TourneyDatabase = dict:from_list(NewDB),
    write_csv_file(Fd, TourneyDatabase, I),
    rank_tourney(TourneyDatabase, Next, Conf, Fd);
rank_tourney(DB, [], _Conf, _Fd) ->
    {ok, DB}.

rank(DB, [Player | Next], Idx, Conf) ->
    Rank = rank_player(DB, Player, Idx, Conf),
    [Rank | rank(DB, Next, Idx, Conf)];
rank(_DB, [], _Idx, _) -> [].

%% Prediction
%% ----------------------------------------------------------------------

%% @doc Gauge how well we predict matches
%% <p>given a database of player matches and a tournament index, go
%% through the match outcomes in the tournament with respect to our
%% ratings. Then run a binomial prediction on how well we guess the
%% correct outcome of the matches for the tournament. That is, the
%% better we are, the better the score.</p>
%% <p>This code is used by the annealer in order to find the best
%% configuration parameters for the system.</p>
%% @end
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

setup_file(Fname) ->
    {ok, Fd} = file:open(Fname, [write]),
    ok = file:write(Fd, ["Tournament,Player,Map,R,RD,Sigma", $\n]),
    Fd.

write_csv_file(Fd, Db, Idx) ->
   T = integer_to_list(Idx),
   Data =
     dict:fold(fun(Id, R, Acc) -> [T, $,, format_player(Id, R), $\n | Acc] end, [], Db),
   ok = file:write(Fd, Data),
   lager:info("Wrote tournament ~B", [Idx]),
   ok.

format_player({Id, Map}, {R, Rd, S}) ->
    [{Id, Name}] = ets:lookup(qlg_players, Id),
    [Name, $,, Map, $,, float_to_list(R), $,, float_to_list(Rd), $,, float_to_list(S)].

name({player, N, _, _, _}) -> N.

commatize([]) -> [];
commatize([E]) -> [E];
commatize([A| Rest]) -> [A, $,, commatize(Rest)].


matrix(Players) ->
    Matrix = [[name(P), $,, commatize([io_lib:format("~6.2. f", [expected_score(P, O)])
                             || O <- Players]), $\n]
              || P <- Players],
    Header = ["Name,", commatize([name(P) || P <- Players]), $\n],
    [Header | Matrix].

