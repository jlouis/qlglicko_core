-module(qlglicko_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = read_configuration(),
    start_dispatcher(),
    qlglicko_sup:start_link().

stop(_State) ->
    ok.


%% ----------------------------------------------------------------------
read_configuration() ->
    gproc:get_set_env(l, qlglicko, postgres, [app_env, error]),
    ok.

start_dispatcher() ->
    ok = dispcount:start_dispatch(
           qlg_db_dispatcher,
           {qlg_db_dispatcher, []},
           [{restart,permanent},{shutdown,4000},
            {maxr,10},{maxt,60},{resources,10}]
          ).
