-module(qlg_fetch_player).

-behaviour(gen_server).

%% API
-export([start_link/2, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { id,
                 name }).

%%%===================================================================

start_link(Id, Name) ->
    gen_server:start_link(?MODULE, [Id, Name], []).

run(Pid) ->
    gen_server:cast(Pid, run).

%%%===================================================================

%% @private
init([Id, Name]) ->
    true = gproc:add_local_name({fetch_player, Name}),
    {ok, #state{ id = Id, name = Name }}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(run, State) ->
    %% @todo handle errors and overloads
    case jobs:ask(ql_fetch) of
        {ok, _Opaque} ->
            fetch_and_store(State),
            {stop, normal, State}
    end;
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

fetch_and_store(#state { id = Id, name = Name}) ->
    lager:debug("Refreshing player ~s", [Name]),
    case qlg_pgsql_srv:should_player_be_refreshed(Id) of
        true ->
            {ok, Matches} = ql_fetch:player_matches(Id),
            [{ok, _} = qlg_pgsql_srv:store_match(M, null) || M <- Matches],
            {ok, 1} = qlg_pgsql_srv:refresh_player(Id),
            ok;
        false ->
            ok
    end.
