-module(qlg_fetch_match).

-include("match.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { id }).

%%%===================================================================

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

run(Pid) ->
    gen_server:cast(Pid, run).

%%%===================================================================

%% @private
init([Id]) ->
    true = gproc:add_local_name({fetch_match, Id}),
    {ok, #state{ id = Id }}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(run, State) ->
    %% @todo Handle errors and overloads
    case jobs:ask(ql_fetch) of
        {ok, _Opaque} ->
            case crypto:rand_uniform(0, 4) of
                0 ->
                    fetch_and_store(State);
                _ ->
                    ok
            end,
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

fetch_and_store(#state { id = Id }) ->
    lager:debug("Fetching match ~p", [Id]),
    {ok, JSON} = ql_fetch:match(Id),
    %% Do this last as a confirmation we got through the other parts
    %% This ensures an idempotent database.
    {ok, 1} = qlg_pgsql_srv:store_match(
                Id,
                term_to_binary(JSON, [compressed])),
    ok.
