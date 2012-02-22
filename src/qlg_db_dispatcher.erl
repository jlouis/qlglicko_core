-module(qlg_db_dispatcher).
-behaviour(dispcount).
-export([init/1,
         checkout/2,
         checkin/2,
         handle_info/2,
         dead/1,
         terminate/2,
         code_change/3]).

-record(state, {resource, given=false}).

init([]) ->
    {ok, C} = qlg_pgsql_srv:db_connect(),
    {ok, #state { resource = C}}.

%% just in case, but that should never happen anyway :V I'm paranoid!
checkout(_From, State = #state{resource= C }) ->
    {ok, C, State#state { given = true }}.

checkin(C, State = #state{resource=C, given=true}) ->
    {ok, State#state{given=false}};
checkin(_Socket, State) ->
    %% The socket doesn't match the one we had -- an error happened somewhere
    {ignore, State}.

dead(#state { resource = C } = State) ->
    %% aw shoot, someone lost our resource, we gotta create a new one:
    ok = pgsql:close(C),
    {ok, NewC} = qlg_pgsql_srv:db_connect(),
    {ok, State#state{ resource = NewC, given = false}}.

handle_info(_Msg, State) ->
    %% something unexpected with the TCP connection if we set it to active,once???
    {ok, State}.

terminate(_Reason, #state { resource = C }) ->
    ok = pgsql:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
