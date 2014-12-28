-module(qlglicko).

-export([rank/1, rank/2]).
-export([fetch_match/1]).

fetch_match(Id) ->
    {ok, M} = qlg_db:fetch_match(Id),
    binary_to_term(M).

rank(Id) ->
    rank(Id, []).

rank(Id, Opts) ->
    qlg_ranker:rank(Id, Opts).

