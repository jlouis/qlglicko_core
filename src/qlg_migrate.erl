-module(qlg_migrate).

-export([migrate/0]).

-define(HOSTNAME, "localhost").
-define(USERNAME, "qlglicko_processing").
-define(PASSWORD, "0okTivlur7").

migrate() ->
	Conn = connect(),
	Prepare = prepare_statements(Conn),
	loop(Conn, Prepare),
	disconnect(Conn).
	
disconnect(Conn) ->
	pgsql:close(Conn).
	
connect() ->
	{ok, Conn} = pgsql:connect(?HOSTNAME, ?USERNAME, ?PASSWORD, [{database, "qlglicko"}]),
	Conn.

loop(Conn, {QueryStmt}) ->
	run_loop(Conn, QueryStmt, pgsql:execute(Conn, QueryStmt, "one", 1*1000)).
	
run_loop(Conn, QueryStmt, {partial, Rows}) ->
	[convert_row(Conn, R) || R <- Rows],
	io:format("."),
	run_loop(Conn, QueryStmt, pgsql:execute(Conn, QueryStmt, "one", 1*1000));
run_loop(Conn, _QueryStmt, {ok, Rows}) ->
	[convert_row(Conn, R) || R <- Rows],
	ok.

prepare_statements(Conn) ->
	{ok, QueryStmt} = 
		pgsql:parse(Conn, "SELECT id, content FROM raw_match"),
	ok = pgsql:bind(Conn, QueryStmt, "one", []),
	{QueryStmt}.

convert_row(Conn, {Id, Content}) ->
	Term = binary_to_term(Content),
	insert(Conn, Id, jsx:encode(Term)).

insert(Conn, Id, JSON) ->
	{ok, 1} = pgsql:equery(
		Conn,
		"INSERT INTO core.raw_match_json (id, content) VALUES ($1, $2)",
		[Id, JSON]).
		

