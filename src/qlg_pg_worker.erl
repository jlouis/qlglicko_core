-module(qlg_pg_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% Lifetime
-export([start_link/1]).

%% Gen Server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% API
-export([squery/2, squery/3, equery/3, equery/4]).

-record(state, {conn}).

squery(Worker, Sql) ->
    gen_server:call(Worker, {squery, Sql}, infinity).

squery(Worker, Sql, Timeout) ->
    gen_server:call(Worker, {squery, Sql}, Timeout).

equery(Worker, Sql, Params) ->
    gen_server:call(Worker, {squery, Sql, Params}, infinity).

equery(Worker, Sql, Params, Timeout) ->
    gen_server:call(Worker, {squery, Sql, Params}, Timeout).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    [Hostname, Database, Username, Password] =
        [proplists:get_value(X, Args)
         || X <- [hostname, database, username, password]],
    {ok, Conn} = pgsql:connect(Hostname, Username, Password,
                               [{database, Database}]),
    {ok, #state{ conn = Conn }}.

handle_call({squery, Sql}, _From, #state { conn = Conn } = State) ->
    {reply, pgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state { conn = Conn } = State) ->
    {reply, pgsql:equery(Conn, Stmt, Params), State};
handle_call(_Requrest, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {stop, impossible, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state { conn = Conn }) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

