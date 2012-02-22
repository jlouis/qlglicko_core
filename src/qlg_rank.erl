-module(qlg_rank).

-export([rank/2, rank/3]).

-define(CHUNK_SIZE, 1000).

rank(T, I) ->
    rank(T, I, []).

rank(Tournament, Info, Options) ->
    ets:new(qlg_rank, [named_table, public]),
    Players = fetch_players(Tournament),
    lager:debug("Ranking ~B players for tournament ~p", [length(Players),
                                                         Tournament]),
    _ = rank_parallel([P || {P} <- Players], [], Tournament, Info),
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
    {ok, Pid} = qlg_ranker_pool:spawn_worker(Tournament, Chunk),
    rank_parallel(Rest,
                  [Pid | Workers], Tournament, Info).


rank_collect(Workers) ->
    lager:debug("Collecting workers to make sure all ranking is done"),
    [qlg_rank_worker:done(Pid) || Pid <- Workers].

fetch_players(Tournament) ->
    {ok, _, Players} = qlg_pgsql_srv:players_in_tournament(Tournament),
    Players.



