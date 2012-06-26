-module(qlg_mem_db).

-export([loader_looper/0]).

-export([load_keep/0, unload_all/0]).

unload_all() ->
    ets:delete(qlg_matches),
    ets:delete(qlg_players).


load_keep() ->
    spawn(fun() ->
                  load_all(),
                  loader_looper()
          end).

loader_looper() ->
    receive stop -> ok
    after 2000 -> ?MODULE:loader_looper()
    end.
        
load_all() ->
    {ok, Ts} = qlg_pgsql_srv:all_tournaments(),
    load_tournaments(Ts),
    {ok, Players} = qlg_pgsql_srv:all_players(),
    load_players(Players),
    {ok, ets:info(qlg_matches, size), ets:info(qlg_players, size)}.

load_players(Players) ->
    ets:new(qlg_players, [named_table, public, {read_concurrency, true}, set]),
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
