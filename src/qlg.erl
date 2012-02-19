-module(qlg).

-export([rank/1, rank/2]).

rank(Id) ->
    rank(Id, []).

rank(Id, Opts) ->
    qlg_ranker:rank(Id, Opts).

