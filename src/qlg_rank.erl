-module(qlg_rank).

-export([rank/1, rank/2, rank/3,
         init_players/0,
         load_all/0,
         unload_all/0,
         load_tournaments/1]).

-define(CHUNK_SIZE, 15000).

unload_all() ->
    ets:delete(qlg_matches).

load_all() ->
    {ok, Ts} = qlg_pgsql_srv:all_tournaments(),
    load_tournaments(Ts),
    {ok, ets:info(qlg_matches, size)}.

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
    PDB = dict:new(),
    rank(PDB, Idxs).

rank(DB, [I | Next]) ->
    OldPlayers = dict:fetch_keys(DB),
    {ok, Players, _L} = init_players(I),
    NewDB = rank(DB, lists:usort(OldPlayers ++ Players), I),
    rank(dict:from_list(NewDB), Next);
rank(DB, []) ->
    {ok, DB}.

rank(DB, [Player | Next], Idx) ->
    Rank = rank_player(DB, Player, Idx),
    [Rank | rank(DB, Next, Idx)];
rank(DB, [], _Idx) -> [].

player_ranking(DB, P) ->
    case dict:find(P, DB) of
        error ->
            {R, RD, Sigma} = gproc:get_env(l, qlglicko, default_ranking,
                                           [app_env, error]),
            {P, R, RD, Sigma};
        {ok, {R, RD, Sigma}} ->
            {P, R, RD, Sigma}
    end.

rank_player(Db, Player, Idx) ->
    {Player, R, RD, Sigma} = player_ranking(Db, Player),
        Wins = [begin
                {_P, LR, LRD, _Sigma} = player_ranking(Db, Opp),
                {LR, LRD, 1}
            end || {_, Opp} <- ets:lookup(qlg_matches, {Idx, {Player, w}})],
    Losses = [begin
                  {_P, WR, WRD, _Sigma} = player_ranking(Db, Opp),
                  {WR, WRD, 0}
              end || {_, Opp} <- ets:lookup(qlg_matches, {Idx, {Player, l}})],
    case Wins ++ Losses of
        [] ->
            RD1 = glicko2:phi_star(RD, Sigma),
            {Player, {R, RD1, Sigma}};
        Opponents ->
            {R1, RD1, Sigma1} =
                glicko2:rate(R, RD, Sigma, Opponents),
            {Player, {R1, RD1, Sigma1}}

    end.

store_tournament_ranking(T) ->
    store_tournament_ranking(T, ets:match_object(qlg_rank, '$1', ?CHUNK_SIZE)).

store_tournament_ranking(T, '$end_of_table') ->
    qlg_pgsql_srv:tournament_mark_ranked(T);
store_tournament_ranking(T, {Matches, Continuation}) ->
    [store_player_ranking(T, P) || P <- Matches],
    store_tournament_ranking(T, ets:match_object(Continuation)).

store_player_ranking(T, {Id, R, RD, Sigma}) ->
    {ok, 1} = qlg_pgsql_srv:store_player_ranking(T, {Id, R, RD, Sigma}).

write_csv() ->
    Ratings = ets:match_object(qlg_rank, '$1'),
    IoData = ["Player,R,RD,Sigma", $\n,
              [[format_player(R), $\n] || R <- Ratings]],
    file:write_file("rankings.csv", IoData).

format_player({P, R, Rd, S}) ->
    Name = qlg_pgsql_srv:fetch_player_name(P),
    [Name, $,,
     float_to_list(R), $,,
     float_to_list(Rd), $,,
     float_to_list(S)].

