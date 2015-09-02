-module(qlglicko_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DBPools = make_pools(),
    OverloadDetect =
        {qlg_overload, {qlg_overload, start_link, []},
         permanent, 5000, worker, [qlg_overload]},
    FetchPlayerPool =
        {qlg_fetch_player_pool, {qlg_fetch_player_pool, start_link, []},
         permanent, infinity, supervisor, [qlg_fetch_player_pool]},
    FetchMatchPool =
        {qlg_fetch_match_pool, {qlg_fetch_match_pool, start_link, []},
         permanent, infinity, supervisor, [qlg_fetch_match_pool]},
    MatchAnalyzer =
        {qlg_match_analyzer, {qlg_match_analyzer, start_link, []},
         permanent, 5000, worker, [qlg_match_analyzer]},
    Timer =
        {qlg_timer, {qlg_timer, start_link, []},
         permanent, 5000, worker, [qlg_timer]},
    {ok, { {one_for_all, 10, 3600},
           [OverloadDetect]
           ++ DBPools
           ++ [FetchPlayerPool,
               FetchMatchPool,
               MatchAnalyzer,
               Timer
              ]} }.


%% --------------------------------------------------
make_pools() ->
    {ok, Pools} = application:get_env(qlglicko_core, db_pools),
    lists:map(
      fun({Name, PoolConfig, WorkerConfig}) ->
              poolboy:child_spec(
                Name,
                [{name, {local, Name}},
                 {worker_module, qlg_pg_worker}] ++ PoolConfig,
                WorkerConfig)
      end,
      Pools).


             

