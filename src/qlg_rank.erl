-module(qlg_rank).

-export([rank/3,
         expected_score/4,
         run/1, eprofile/0, fprofile/0,
         predict/2,
         rate_game/2,
         matrix/1,
         write_player_csv/2,
         write_matrix/2
        ]).

-export([load_keep/1, unload_all/0, loader_looper/0]).
-compile([{native, o3}, inline]).

%% Loading
%% ----------------------------------------------------------------------

unload_all() ->
    ets:delete(qlg_matches_c),
    ets:delete(qlg_new_players).

load_keep(Idxs) ->
    spawn(fun() ->
                  {ok, Matches, Players} = load_all(Idxs),
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
        
load_all(Idxs) ->
    {ok, Ts} = qlg_pgsql_srv:all_tournaments(),
    load_tournaments(Ts),
    {ok, Players} = qlg_pgsql_srv:all_players(),
    load_players(Players),
    setup_new_player_table(Idxs),
    coalesce_match_table(),
    {ok, ets:info(qlg_matches_c, size), ets:info(qlg_players, size)}.

setup_new_player_table(Idxs) ->
    ets:new(qlg_new_players, [named_table, public, {read_concurrency, true}, set]),
    setup_new_player_table_m(Idxs, sets:new()),
    ok.
    
setup_new_player_table_m([], _S) ->
    ok;
setup_new_player_table_m([Idx | Next], S) ->
    NewPlayers = sets:from_list([P || [P] <- ets:match(qlg_matches, {{Idx, {'$1', '_'}}, '_'})]),
    Added = sets:subtract(NewPlayers, S),
    ets:insert_new(qlg_new_players, {Idx, sets:to_list(Added)}),
    lager:info("Tourney ~B adds ~B new players", [Idx, sets:size(Added)]),
    setup_new_player_table_m(Next, sets:union(S, NewPlayers)).

coalesce_match_table() ->
    lager:info("Coalescing"),
    ets:new(
        qlg_matches_c,
        [named_table, public, {read_concurrency, true}, set, protected]),
    coalesce(ets:match(qlg_matches, {'$1', '_'}, 20000)),
    ets:delete(qlg_matches),
    ok.
    
coalesce('$end_of_table') -> ok;
coalesce({Keys, Cont}) ->
    [begin
        Opps = ets:lookup_element(qlg_matches, K, 2),
        ets:insert(qlg_matches_c, {K, Opps})
     end || [K] <- lists:usort(Keys)],
    io:format("."),
    coalesce(ets:match(Cont)).


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
    Players = ets:lookup_element(qlg_new_players, Idx, 2),
    {ok, Players}.

player_ranking(DB, P, Conf) ->
    case dict:find(P, DB) of
        error ->
            {RD, Sigma, _} = glicko2:read_config(Conf),
            R = 1500,
            {P, R, RD, Sigma};
        {ok, {R, RD, Sigma}} ->
            {P, R, RD, Sigma}
    end.

clamp(Lo, X, _ ) when X < Lo -> Lo;
clamp(_,  X, Hi) when X > Hi -> Hi;
clamp(_,  X, _ ) -> X.


rank_player(Db, Player, {R, RD, Sigma}, Idx, Conf) ->
    WinOpp =
    	try ets:lookup_element(qlg_matches_c, {Idx, {Player, w}}, 2) of
    		V1 -> V1
    	catch _:_ -> []
    	end,
    Wins = [begin
                {_P, LR, LRD, _Sigma} = player_ranking(Db, Opp, Conf),
                {LR, LRD, 1}
            end || Opp <- WinOpp ],
    LossOpp =
      	try ets:lookup_element(qlg_matches_c, {Idx, {Player, l}}, 2) of
    		V2 -> V2
    	catch _:_ -> []
    	end,
    Losses = [begin
                  {_P, WR, WRD, _Sigma} = player_ranking(Db, Opp, Conf),
                  {WR, WRD, 0}
              end || Opp <- LossOpp ],
    case Wins ++ Losses of
        [] ->
            RD1 = glicko2:phi_star(RD, Sigma),
            {R, RD1, Sigma};
        Opponents ->
            {R1, RD1, Sigma1} =
                glicko2:rate(R, RD, Sigma, Opponents, Conf),
            {clamp(0.0, R1, 3000.0),
                      clamp(0.0, RD1, 400.0),
                      clamp(0.0, Sigma1, 0.1)}
    end.

eprofile() ->
	eprof:profile(fun() -> qlg_rank:run(lists:seq(1,41)) end),
	eprof:stop_profiling(),
	eprof:log("eprof.run"),
	eprof:analyze().

fprofile() ->
    fprof:apply(qlg_rank, run, [lists:seq(1,2)]),
    fprof:profile(),
    fprof:analyse([{dest, "fprof.out"}, {cols, 150}, details]).

run(Idxs) ->
    run(Idxs, "rankings.csv").

run(Idxs, no_file) ->
    rank(Idxs, glicko2:configuration(413, 0.08, 0.345), no_file);
run(Idxs, FName) ->
    Fd = setup_file(FName),   
    {ok, Db} = rank(Idxs, glicko2:configuration(413, 0.08, 0.345), Fd),
    file:close(Fd),
    {ok, Db}.

rank(Idxs, Conf, Fd) ->
    PDB = dict:new(),
    rank_tourney(PDB, Idxs, Conf, Fd).

rank_tourney(DB, [I | Next], Conf, Fd) ->
    {RD, Sigma, _} = glicko2:read_config(Conf),
    {ok, Players} = init_players(I),
    NewPlayerDB = dict:from_list([{K, {1500, RD, Sigma}} || K <- Players]),
    F = fun(_K, V1, _V2) -> V1 end,
    UpdatedDB = rank_player_dict(dict:merge(F, DB, NewPlayerDB), I, Conf),
    write_csv_file(Fd, UpdatedDB, I),
    rank_tourney(UpdatedDB, Next, Conf, Fd);
rank_tourney(DB, [], _Conf, _Fd) ->
    {ok, DB}.

rank_player_dict(DB, Idx, Conf) ->
    dict:map(fun(Player, Rank) -> rank_player(DB, Player, Rank, Idx, Conf) end, DB).

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
    Matches = predict_get_matches(Idx),
    Predicted = predict_matches(DB, [[W, L] || [W, M] <- Matches, L <- M]),
    lists:sum(Predicted) / length(Predicted).

predict_get_matches([Idx | Next]) ->
	Ms = ets:match(qlg_matches_c, {{Idx, {'$1', w}}, '$2'}),
	Ms ++ predict_get_matches(Next);
predict_get_matches([]) -> [].
	
rate_game(Y, E) ->
    -(Y * math:log10(E) + (1 - Y)*math:log10(1-E)).

q() ->
    math:log(10) / 400.

expected_g(RD) ->
    1 / (math:sqrt(1 + 3*q()*q()*RD*RD/(math:pi()*math:pi()))).

expected_score({player, _A, RA, RDA, _}, {player, _B, RB, RDB, _}) ->
    expected_score(RA, RDA, RB, RDB).

expected_score(WR, WRD, LR, LRD) ->
    GVal = expected_g(math:sqrt(WRD*WRD + LRD*LRD)),
    1 / (1 + math:pow(10, -GVal * (WR - LR) / 400)).

predict_matches(_Db, []) -> [];
predict_matches(Db, [[Winner, Loser] | Next]) ->
    case dict:find(Winner, Db) of
      error -> predict_matches(Db, Next);
      {ok, {WR, WRD, _}} ->
        case dict:find(Loser, Db) of
          error -> predict_matches(Db, Next);
          {ok, {LR, LRD, _}} ->
            E = expected_score(WR, WRD, LR, LRD),
            [rate_game(1, E) | predict_matches(Db, Next)]
        end
    end.

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

write_csv_file(no_file, _Db, _Idx) -> ok;
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

