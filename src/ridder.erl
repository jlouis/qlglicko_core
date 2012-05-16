-module(ridder).

-export([i_compute_volatility/6]).

vol_f(Phi, V, Delta, A, Tau) ->
    PHI2 = Phi*Phi,
    fun(X) ->
            EX = math:exp(X),
            D2 = Delta*Delta,
            A2 = (PHI2 + V + EX),
            P2 = (X - A) / (Tau * Tau),
            P1 = (EX * (D2 - PHI2 - V - EX))  / (2*A2*A2),
            P1 - P2
    end.

vol_k(K, F, A, Tau) ->
    Const = A - K*math:sqrt(Tau*Tau),
    case F(Const) < 0 of
        true ->
            vol_k(K+1, F, A, Tau);
        false ->
            Const
    end.

i_compute_volatility(Sigma, Phi, V, Delta, Tau, K) ->
    io:format("Volatility starting:~n"),
    io:format("Sigma: ~p, Phi: ~p, V: ~p, Delta: ~p, Tau: ~p~n",
              [Sigma, Phi, V, Delta, Tau]),
    A = math:log(Sigma*Sigma),
    F = vol_f(Phi, V, Delta, A, Tau),
    B = case Delta*Delta > Phi*Phi + V of
            true ->
                io:format("Delta2 > Phi2 + V~n"),
                math:log(Delta*Delta - Phi*Phi - V);
            false ->
                io:format("Iterating k~n"),
                vol_k(1, F, A, Tau)
        end,
    FA = F(A),
    FB = F(B),

    io:format("A: ~p, B: ~p, Fa: ~p, Fb: ~p~n", [A, B, FA, FB]),
    try
        compute_volatility(A, B, F, FA, FB, K)
    catch
        throw:{iterations_exceeded, Vals} ->
            lager:error("Error in vol comp: ~p", [[{sigma, Sigma}, {phi, Phi},
                                                   {delta, Delta}, {tau, Tau},
                                                   {values, Vals} ]]),
            exit(bad_vol_comp)
    end.

sign(X) when X > 0 -> 1;
sign(X) when X < 0 -> -1;
sign(0.0)          ->  0.

-define(EPSILON, 0.000001).

compute_volatility(A, B, F, FA, FB, 0) ->
    throw({iterations_exceeded, {A, B, F, FA, FB}});
compute_volatility(A, B, _F, _FA, _FB, _) when abs(B - A) =< ?EPSILON ->
    math:exp(A/2);
compute_volatility(A, B, F, FA, FB, K) ->
    io:format("[ ~6.. B ] - A: ~p, B: ~p, Fa: ~p, Fb: ~p~n",
              [K, A, B, FA, FB]),
    %% C is the midpoint:
    C = (A + B) * 0.5, FC = F(C),
    D = C + (C - A) * (sign(FA - FB) * FC) / math:sqrt(FC*FC - FA*FB),
    FD = F(D),
    io:format("         C: ~p, Fc: ~p D: ~p Fd: ~p~n", [C, FC, D, FD]),
    case sign(FD) /= sign(FC) of
        true ->
            compute_volatility(C, D, F, FC, FD, K-1);
        false ->
            case sign(FD) /= sign(FA) of
                true ->
                    compute_volatility(A, D, F, FA, FD, K-1);
                false ->
                    true = sign(FD) /= sign(FB),
                    compute_volatility(D, B, F, FD, FB, K-1)
            end
    end.
