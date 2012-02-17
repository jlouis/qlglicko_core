%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <jlouis@myrddraal>
%%% @copyright (C) 2012, Jesper Louis andersen
%%% @doc Overload Detector for QuakeLive.com
%%% @end
%%%-------------------------------------------------------------------
-module(qlg_overload).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
         ask/0,
         overload/0,
         timeout/0]).

%% gen_fsm callbacks
-export([init/1,
         operating/2, operating/3,
         overloaded/2, overloaded/3,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, { fuse = 0,
                 overload_count = 0 }).

-define(OVERLOAD_TIME, 300*1000).
-define(TIMEOUT_TIME, 900*1000).
-define(TICK_TIME, 60*1000).
%%%===================================================================

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

ask() ->
    gen_fsm:sync_send_event(?SERVER, ask).

overload() ->
    gen_fsm:sync_send_event(?SERVER, overload).

timeout() ->
    gen_fsm:sync_send_event(?SERVER, timeout).

%%%===================================================================

%% @private
init([]) ->
    gen_fsm:start_timer(?TICK_TIME, tick),
    {ok, operating, #state{ }}.

%% @private
operating({timeout, _, tick}, State) ->
    gen_fsm:start_timer(?TICK_TIME, tick),
    {next_state, operating, reset(State)};
operating(Event, State) ->
    lager:error("Unknown event: ~p", [Event]),
    {next_state, operating, State}.

%% @private
overloaded({timeout, _, tick}, #state { fuse = blown } = State) ->
    {next_state, overloaded, State};
overloaded({timeout, _, tick}, State) ->
    gen_fsm:start_timer(?TICK_TIME, tick),
    {next_state, overloaded, reset(State)};
overloaded({timeout, _, reset_overload}, State) ->
    lager:info("Resuming operations from overload"),
    {next_state, operating, State};
overloaded({timeout, _, reset_timeout}, State) ->
    lager:info("Resuming operations from timeout"),
    {next_state, operating, State};
overloaded(Event, State) ->
    lager:error("Unknown event: ~p", [Event]),
    {next_state, overloaded, State}.

%% @private
operating(ask, _From, #state { fuse = 75 } = State) ->
    lager:emergency("Fuse blown!"),
    {reply, no, overloaded, State#state { fuse = blown }};
operating(ask, _From, #state { fuse = blown }) ->
    exit(invariant_broken);
operating(ask, _From, #state { fuse = K } = State) ->
    Reply = draw(),
    {reply, Reply, operating, State#state { fuse = K+1 }};
operating(overload, _From, #state { overload_count = 2 } = State) ->
    gen_fsm:start_timer(?OVERLOAD_TIME, reset_overload),
    lager:info("QL overloaded, 5 minutes rest"),
    {reply, ok, overloaded, State};
operating(overload, _From, #state { overload_count = K} = State) ->
    {reply, ok, operating, State#state { overload_count = K+1}};
operating(timeout, _From, #state {} = State) ->
    gen_fsm:start_timer(?TIMEOUT_TIME, reset_timeout),
    lager:info("QL timeout or error, 15 minutes rest"),
    {reply, ok, overloaded, State};
operating(Event, _From, State) ->
    lager:error("Unknown event: ~p", [Event]),
    {reply, ok, operating, State}.

overloaded(ask, _From, #state { } = State) ->
    {reply, no, overloaded, State};
overloaded(timeout, _From, State) ->
    {reply, ok, overloaded, State};
overloaded(overload, _From, State) ->
    {reply, ok, overloaded, State};
overloaded(Event, _From, State) ->
    lager:error("Unknown event: ~p", [Event]),
    {reply, ok, operating, State}.

%% @private
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% @private
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================

reset(#state { } = State) ->
    State#state { fuse = 0, overload_count = 0 }.

draw() ->
    case crypto:rand_uniform(0, 5) of
        0 ->
            yes;
        _ ->
            no
    end.
