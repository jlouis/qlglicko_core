-module(qlg_migrate).

-export([migrate/0]).

-define(HOSTNAME, "localhost").
-define(USERNAME, "qlglicko_processing").
-define(PASSWORD, "0okTivlur7").

migrate() ->
	Conn = connect(),
	epgsql:equery(Conn, "BEGIN"),
	setup(Conn),
	loop(Conn),
	epgsql:equery(Conn, "COMMIT"),
	disconnect(Conn).
	
disconnect(Conn) ->
	epgsql:close(Conn).
	
connect() ->
	{ok, Conn} = epgsql:connect(?HOSTNAME, ?USERNAME, ?PASSWORD, [{database, "qlglicko"}]),
	Conn.

loop(Conn) ->
	run_loop(Conn).
	
run_loop(Conn) ->
	{ok, Rows} = fetch(Conn),
	[convert_row(Conn, R) || R <- Rows],
	io:format("."),
	case Rows of
 		[] -> ok;
		_ -> run_loop(Conn)
	end.

convert_row(Conn, {Id, Content}) ->
	Term = binary_to_term(Content),
	insert(Conn, Id, jsx:encode(Term)).

setup(Conn) ->
	{ok, [], []} = epgsql:equery(
		Conn,
		"DECLARE read_terms CURSOR FOR SELECT id, content FROM raw_match", []),
	ok.

fetch(Conn) ->
	case epgsql:equery(
		Conn,
		"FETCH 10000 FROM read_terms",
		[]) of
		{ok, _, _, Rows} -> {ok, Rows};
		{ok, 0} -> {ok, []}
	end.

insert(Conn, Id, JSON) ->
	{ok, 1} = epgsql:equery(
		Conn,
		"INSERT INTO core.raw_match_json (id, content) VALUES ($1, $2)",
		[Id, JSON]).
		

