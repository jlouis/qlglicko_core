-module(annealing).
-compile([{native, o3}, inline]).
-compile({inline_size, 50}).

-export([anneal/0, anneal/2]).

-define(KMAX, 500).
-define(EMAX, 0.000000000001).

anneal() ->
    S0 = glicko2:configuration(200, 0.06, 0.5),
    anneal(S0).

anneal(S0) ->
    anneal(S0, 8).

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
    controller_loop(Pids, S0, e(S0), 0).

controller_loop([], IS, IE, _) -> {IS, IE};
controller_loop(Pids, IS, IE, K) when is_list(Pids) ->
    receive
        {new_incumbent, SB, EB} when IE > EB ->
            io:format("Improved incumbent: ~p Energy: ~p~n", [SB, EB]),
            [P ! {new_incumbent, SB, EB} || P <- Pids],
            controller_loop(Pids, SB, EB, K);
        {new_incumbent, _SB, _EB}            -> controller_loop(Pids, IS, IE, K);
        {done, Pid}                          -> controller_loop(Pids -- [Pid], IS, IE, K);
        inc when (K rem 10) == 0 -> io:format("~B", [K]),
                                    controller_loop(Pids, IS, IE, K+1);
        inc -> io:format("."),
               controller_loop(Pids, IS, IE, K+1)
    end.

iterate(0, _F, S) -> S;
iterate(N, F, S) -> iterate(N-1, F, F(S)).

anneal_worker(Ctl, S0) ->
    S = iterate(10, fun neighbour/1, S0),
    E = e(S),
    anneal_check(Ctl, S, E, S, E, 0).

anneal_check(Ctl, S, E, SB, EB, K) ->
    receive {new_incumbent, NSB, NEB} -> anneal(Ctl, S, E, NSB, NEB, K)
    after 0                           -> anneal(Ctl, S, E, SB, EB, K)
    end.

anneal(Ctl, S, E, SB, EB, K) when K < ?KMAX, E > ?EMAX ->
    Ctl ! inc,
    T = temperature(1, K),
    SN = neighbour(S),
    try
        EN = e(SN),
        {NextS, NextE} = case p(E, EN, T) > rand:uniform() of
                             true  -> {SN, EN};
                             false -> {S, E}
                         end,
        {NextSB, NextEB} = case EN < EB of
                               true  -> Ctl ! {new_incumbent, SN, EN},
                                        {SN, EN};
                               false -> {SB, EB}
                           end,
        anneal_check(Ctl, NextS, NextE, NextSB, NextEB, K+1)
    catch
        _:_ ->
            io:format("Error in config: ~p~n", [SN]),
            anneal_check(Ctl, S, E, SB, EB, K+1)
    end;
anneal(Ctl, _, _, _, _, _) -> Ctl ! {done, self()}.

temperature(T0, K) -> temperature(exp, T0, K).

temperature(exp, T0, K) -> T0 * math:pow(0.995, K);
temperature(fast, T0, K) -> T0 / K;
temperature(boltz, T0, K) -> T0 / math:log(K).

walk_rd(RD) -> walk(RD, 100).
walk_sigma(Sigma) -> walk(Sigma, 0.008).
walk_tau(Tau) -> walk(Tau, 0.2).

walk(S, Scale) ->
    case rand:uniform() of
        K when K > 0.5 -> S+(rand:uniform() * Scale);
        _ -> S-(rand:uniform() * Scale)
    end.

clamp(Lo, _Hi, X) when X < Lo -> Lo;
clamp(_Lo, Hi, X) when X > Hi -> Hi;
clamp(_, _, X)                -> X.

neighbour(S) ->
    {RD, Sigma, Tau} = glicko2:read_config(S),
    Cnf = glicko2:configuration(
            clamp(15, 500, walk_rd(RD)),
            clamp(0.02, 0.08, walk_sigma(Sigma)),
            clamp(0.2, 1.2, walk_tau(Tau))),
    Cnf.

p(E, NewE, _T) when NewE < E -> 1.0;
p(E, NewE, T ) -> math:exp((E - NewE) / T).

e(S) ->
    {ok, Db} = qlg_rank:rank(lists:seq(1,40), S, no_file),
    qlg_rank:predict(Db, [41, 42, 43]).

