
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
    FetchPlayerPool =
        {qlg_fetch_player_pool, {qlg_fetch_player_pool, start_link, []},
         transient, infinity, supervisor, [qlg_fetch_player_pool]},
    FetchMatchPool =
        {qlg_fetch_match_pool, {qlg_fetch_match_pool, start_link, []},
         transient, infinity, supervisor, [qlg_fetch_match_pool]},
    PgsqlSrv =
        {qlg_pgsql_srv, {qlg_pgsql_srv, start_link, []},
         permanent, 5000, worker, [qlg_pgsql_srv]},
    MatchAnalyzer =
        {qlg_match_analyzer, {qlg_match_analyzer, start_link, []},
         permanent, 5000, worker, [qlg_match_analyzer]},
    Timer =
        {qlglicko_timer, {qlglicko_timer, start_link, []},
         permanent, 5000, worker, [qlglicko_timer]},
    {ok, { {one_for_one, 3, 3600}, [FetchPlayerPool,
                                    FetchMatchPool,
                                    PgsqlSrv,
                                    MatchAnalyzer,
                                    Timer]} }.


