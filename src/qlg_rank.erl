-module(qlg_rank).

-export([rank/2, rank/3,
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
    
rank(T, I) ->
    rank(T, I, []).

rank(Tournament, Info, Options) ->
    ets:new(qlg_rank, [named_table, public]),
    ets:new(qlg_player_ratings,
            [named_table, public, {read_concurrency, true}]),
    Players = fetch_players(Tournament),
    lager:debug("Ranking ~B players for tournament ~p", [length(Players),
                                                         Tournament]),
    lager:debug("Number of matches in tournament: ~B",
                [ets:info(qlg_matches, size)]),
    _ = rank_parallel(Players, [], Tournament, Info),
    case proplists:get_value(write_csv, Options) of
        undefined ->
            ok;
        true ->
            lager:debug("Writing CSV file"),
            ok = write_csv()
    end,
    case proplists:get_value(save_tournament, Options) of
        undefined ->
            ok;
        true ->
            lager:debug("Store rankings in DB"),
            store_tournament_ranking(Tournament)
    end,
    ets:delete(qlg_rank),
    ets:delete(qlg_player_ratings),
    ets:delete(qlg_matches),
    ok.


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

rank_parallel([], Workers, _Tournament, _Info) ->
    rank_collect(Workers);
rank_parallel(Players, Workers, Tournament, Info) when is_list(Players) ->
    {Chunk, Rest} =
        try
            lists:split(?CHUNK_SIZE, Players)
        catch
            error:badarg ->
                {Players, []}
        end,
    lager:debug("Spawning a parallel ranking job"),
    {ok, Pid} = qlg_ranker_pool:spawn_worker(Chunk),
    rank_parallel(Rest,
                  [Pid | Workers], Tournament, Info).


rank_collect(Workers) ->
    lager:debug("Collecting workers to make sure all ranking is done"),
    [qlg_rank_worker:done(Pid) || Pid <- Workers].

fetch_players(Tournament) ->
    {ok, _, Players} = qlg_pgsql_srv:players_in_tournament(Tournament),
    [begin
         ets:insert(qlg_player_ratings, P),
         Name
     end || {Name, _, _, _} = P <- Players].


