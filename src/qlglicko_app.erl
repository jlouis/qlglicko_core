-module(qlglicko_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = read_configuration(),
    qlglicko_sup:start_link().

stop(_State) ->
    ok.


%% ----------------------------------------------------------------------
read_configuration() ->
    gproc:get_set_env(l, qlglicko, riak_host, [app_env, error]),
    ok.
