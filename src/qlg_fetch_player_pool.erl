-module(qlg_fetch_player_pool).

-behaviour(supervisor).

%% API
-export([start_link/0,
         fetch_player/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================

%% @doc
%% Starts the supervisor
%% @end
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

fetch_player(Id, Name, Age) ->
    case gproc:lookup_local_name({fetch_player, Name}) of
        undefined ->
            {ok, Pid} = supervisor:start_child(?SERVER, [Id, Name, Age]),
            qlg_fetch_player:run(Pid),
            {ok, Pid};
        P when is_pid(P) ->
            {error, {already_started, Name}}
    end.

%%%===================================================================

%% @private
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 20000,
    Type = worker,

    FetchChild = {fetch_child, {qlg_fetch_player, start_link, []},
                  Restart, Shutdown, Type, [qlg_fetch_player]},

    {ok, {SupFlags, [FetchChild]}}.

%%%===================================================================
