-module(glicko_bg).

-export([initialize/2,
         delete/1,
         average_matches/2,
         matches/2]).

initialize(Players, Matches) ->
    G = digraph:new(),
    [digraph:add_vertex(G, P) || P <- Players],
    add_matches(G, Matches).

match_count(G, [P]) ->
    length(digraph:in_edges(G, P)) + length(digraph:out_edges(G, P)).

average_matches(G, Players) ->
    Counts = [match_count(G, P) || P <- Players,
                                   match_count(G, P) /= 0],
    lists:sum(Counts) / length(Counts).

delete(G) ->
    true = digraph:delete(G).

add_matches(G, []) -> G;
add_matches(G, [{Winner, Loser, N} | Next]) ->
    digraph:add_edge(G, Winner, Loser, N),
    add_matches(G, Next).

matches(Graph, Player) ->
    Won = [begin
               {_, _W, L, N} = digraph:edge(Graph, E),
               [{L, R, RD, _}] = glicko_db:lookup(L),
               {R, RD, N, 1}
           end || E <- digraph:out_edges(Graph, Player)],
    Lost = [begin
                {_, W, _L, N} = digraph:edge(Graph, E),
                [{W, R, RD, _}] = glicko_db:lookup(W),
                {R, RD, N, 0}
            end || E <- digraph:in_edges(Graph, Player)],
    Matches = explode(Won) ++ explode(Lost),
    Matches.

explode(X) ->
    lists:concat(explode_1(X)).

explode_1([]) -> [];
explode_1([{R, RD, No, C} | Next]) ->
    [ [{R, RD, C} || _ <- lists:seq(1, No)] | explode_1(Next) ].

