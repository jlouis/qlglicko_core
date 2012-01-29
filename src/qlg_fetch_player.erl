-module(qlg_fetch_player).

-behaviour(gen_server).

%% API
-export([start_link/1, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { name }).

%%%===================================================================

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

run(Pid) ->
    gen_server:cast(Pid, run).

%%%===================================================================

%% @private
init([Name]) ->
    true = gproc:add_local_name({fetch_player, Name}),
    {ok, #state{ name = Name }}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(run, State) ->
    fetch_and_store(State),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

fetch_and_store(#state { name = Name}) ->
    {ok, Matches} = ql_fetch:player_matches(Name),
    [{ok, _} = qlg_pgsql_srv:store_match(M, null) || M <- Matches],
    {ok, 1} = qlg_pgsql_srv:refresh_player(Name),
    ok.
