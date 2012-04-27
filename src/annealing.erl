-module(annealing).

-export([anneal/1, anneal/2]).

-define(KMAX, 2500).
-define(EMAX, 0.00001).
-define(INITIAL_TEMP, 100000).

anneal(S0) ->
    anneal(S0, 4).

anneal(S0, K) ->
    Originator = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
                       ets:new(anneal_incumbent, [named_table, protected,
                                                  {read_concurrency, true}]),
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
    ets:insert(anneal_incumbent, {incumbent, S0, e(S0)}),
    controller_loop(Pids).

controller_loop([]) ->
    Res = ets:lookup(anneal_incumbent, incumbent),
    Res;
controller_loop(Pids) when is_list(Pids) ->
    receive
        {new_incumbent, SB, EB} ->
            case ets:lookup(anneal_incumbent, incumbent) of
                [{incumbent, _, OEB}] when OEB > EB ->
                    ets:insert(anneal_incumbent, {incumbent, SB, EB});
                [{incumbent, _, _}] ->
                    ok
            end,
            controller_loop(Pids);
        {done, Pid} ->
            controller_loop(Pids -- [Pid])
    end.

anneal_worker(Ctl, S0) ->
    sfmt:seed(os:timestamp()),
    S = S0,
    E = e(S),
    anneal(Ctl, S, E, S, E, 0).

anneal(Ctl, S, E, SB, EB, K) when K < ?KMAX, E > ?EMAX ->
    T = temperature(K / ?KMAX),
    SN = neighbour(S),
    EN = e(SN),
    {NextS, NextE} = case p(E, EN, T) > sfmt:uniform() of
                         true ->
                             {SN, EN};
                         false ->
                             {S, E}
                     end,
    {NextSB, NextEB} = case EN < EB of
                           true ->
                               Ctl ! {new_incumbent, SN, EN},
                               {SN, EN};
                           false -> {SB, EB}
                       end,
    anneal(Ctl, NextS, NextE, NextSB, NextEB, K+1);
anneal(Ctl, _, _, _, _, _) -> Ctl ! {done, self()}.

temperature(X) ->
    (1 - X). %% This is very naive at the moment.

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
