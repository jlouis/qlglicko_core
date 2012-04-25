-module(annealing).

-export([anneal/1]).

-define(KMAX, 1000).
-define(EMAX, 0.00001).
-define(INITIAL_TEMP, 100000).

anneal(S0) ->
    random:seed(os:timestamp()),
    S = S0,
    E = e(S),
    SB = S,
    EB = E,
    anneal(S, E, SB, EB, 0).

anneal(S, E, SB, EB, K) when K < ?KMAX, E > ?EMAX ->
    T = temperature(K / ?KMAX),
    SN = neighbour(S),
    EN = E(SN),
    {NextS, NextE} = case p(E, EN, T) > random:uniform() of
                         true -> {SN, EN};
                         false -> {S, E}
                     end,
    {NextSB, NextEB} = case E < EB of
                           true -> {SN, EN};
                           false -> {SB, EB}
                       end,
    anneal(NextS, NextE, NextSB, NextEB, K+1);
anneal(_, _, SB, _, _) -> SB.

temperature(X) ->
    (1 - X). %% This is very naive at the moment.

f(X) ->
    (X + 4) * (X + 1) * (X - 2) / 4.0.

walk(S) ->
    case random:uniform() of
        K when K > 0.5 ->
            S+(random:uniform() / 10.0);
        _ ->
            S-(random:uniform() / 10.0)
    end.

clamp(Lo, _Hi, X) when X < Lo -> Lo;
clamp(_Lo, Hi, X) when X > Hi -> Hi;
clamp(_, _, X)                -> X.

neighbour(S) ->
    clamp(-6, 6, walk(S)).
%%    qlglicko:new_candidate(S).

p(E, NewE, _T) when NewE < E -> 1.0;
p(E, NewE, T ) -> math:exp((E - NewE) / T).

e(S) ->
    10 - f(S).
    %% PR = qlglicko:predict(S),
    %% 1.0 - PR.
