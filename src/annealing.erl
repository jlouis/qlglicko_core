-module(annealing).

-export([anneal/1, anneal/2]).

-define(KMAX, 1000).
-define(EMAX, 0.00001).
-define(GRACE, 500).

anneal(S0) ->
    anneal(S0, 4).

anneal(S0, K) ->
    Originator = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
                       Res = controller(S0, K),
                       Originator ! {Ref, Res}
               end),
    receive
        {Ref, R} ->
            {ok, R}
    end.

controller(S0, K) ->
    Ctl = self(),
    Pids = [spawn_link(fun() ->
                               anneal_worker(Ctl, S0)
                       end) || _ <- lists:seq(1,K)],
    controller_loop(Pids, S0, e(S0)).

controller_loop([], IS, IE) ->
    {IS, IE};
controller_loop(Pids, IS, IE) when is_list(Pids) ->
    receive
        {new_incumbent, SB, EB} when IE > EB ->
            io:format("Improved incumbent: ~p Energy: ~p~n", [SB, EB]),
            [P ! {new_incumbent, SB, EB} || P <- Pids],
            controller_loop(Pids, SB, EB);
        {new_incumbent, _SB, _EB}            ->
            controller_loop(Pids, IS, IE);
        {done, Pid}                          ->
            controller_loop(Pids -- [Pid], IS, IE)
    end.

anneal_worker(Ctl, S0) ->
    sfmt:seed(os:timestamp()),
    S = S0,
    E = e(S),
    anneal_check(Ctl, S, E, S, E, 0).

anneal_check(Ctl, S, E, SB, EB, K) ->
    receive {new_incumbent, NSB, NEB} -> anneal(Ctl, S, E, NSB, NEB, K)
    after 0                           -> anneal(Ctl, S, E, SB, EB, K)
    end.

anneal(Ctl, S, E, SB, EB, K) when K < ?KMAX, E > ?EMAX ->
    T = temperature(1, K),
    SN = neighbour(S),
    EN = e(SN),
    {NextS, NextE} = case p(E, EN, T) > sfmt:uniform() of
                         true  -> {SN, EN};
                         false -> {S, E}
                     end,
    {NextSB, NextEB} = case EN < EB of
                           true  ->
                               Ctl ! {new_incumbent, SN, EN},
                               {SN, EN};
                           false ->
                               {SB, EB}
                       end,
    anneal_check(Ctl, NextS, NextE, NextSB, NextEB, K+1);
anneal(Ctl, _, _, _, _, _) -> Ctl ! {done, self()}.

temperature(T0, K) ->
    temperature(exp, T0, K).

temperature(exp, T0, K) ->
    T0 * math:pow(0.995, K);
temperature(fast, T0, K) ->
    T0 / K;
temperature(boltz, T0, K) ->
    T0 / math:log(K).

f(X) ->
    (X + 4) * (X + 1) * (X - 2) / 4.0.

walk(S) ->
    case sfmt:uniform() of
        K when K > 0.5 ->
            S+(sfmt:uniform() * 2);
        _ ->
            S-(sfmt:uniform() * 2)
    end.

clamp(Lo, _Hi, X) when X < Lo -> Lo;
clamp(_Lo, Hi, X) when X > Hi -> Hi;
clamp(_, _, X)                -> X.

neighbour(S) ->
    N = walk(S),
    clamp(-6, 6, N).
%%    qlglicko:new_candidate(S).

p(E, NewE, _T) when NewE < E -> 1.0;
p(E, NewE, T ) -> math:exp((E - NewE) / T).

e(S) ->
    100 - f(S).
    %% PR = qlglicko:predict(S),
    %% 1.0 - PR.
