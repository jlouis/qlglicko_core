-module(qlg_fetch_match_pool).

-behaviour(supervisor).

%% API
-export([start_link/0,
         fetch_match/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================

%% @doc
%% Starts the supervisor
%% @end
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

fetch_match(Id) ->
    case gproc:lookup_local_name({fetch_match, Id}) of
        undefined ->
            {ok, Pid} = supervisor:start_child(?SERVER, [Id]),
            qlg_fetch_match:run(Pid),
            {ok, Pid};
        P when is_pid(P) ->
            {error, {already_started, Id, P}}
    end.

%%%===================================================================

%% @private
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 20000,
    Type = worker,

    FetchChild = {fetch_child, {qlg_fetch_match, start_link, []},
                  Restart, Shutdown, Type, [qlg_fetch_match]},

    {ok, {SupFlags, [FetchChild]}}.

%%%===================================================================
