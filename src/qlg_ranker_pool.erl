%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <jlouis@tiefling.local>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc Pool for Ranker processes
%%% @end
%%% Created : 22 Feb 2012 by Jesper Louis Andersen <jlouis@tiefling.local>
%%%-------------------------------------------------------------------
-module(qlg_ranker_pool).

-behaviour(supervisor).

%% API
-export([start_link/1, spawn_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================

%% @doc
%% Starts the supervisor
%% @end
start_link(DispatchInfo) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DispatchInfo]).

spawn_worker(Ps) ->
    {ok, Pid} = supervisor:start_child(?SERVER, []),
    qlg_rank_worker:run(Pid, Ps),
    {ok, Pid}.

%%%===================================================================

%% @private
init([DispatchInfo]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    RankWorker = {rank_worker,
                  {qlg_rank_worker, start_link, [DispatchInfo]},
                  Restart, Shutdown, Type, [qlg_rank_worker]},

    {ok, {SupFlags, [RankWorker]}}.

%%%===================================================================
