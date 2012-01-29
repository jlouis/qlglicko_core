-module(glicko2).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([phi_star/2,
         rate/4]).

-define(TAU, 0.5). % Good values are between 0.3 and 1.2
-define(EPSILON, 0.0000001).

square(V) -> V*V.

%% Step 2
scale(R, RD) ->
    Mu = (R - 1500) / 173.7178,
    Phi = RD / 173.7178,
    {Mu, Phi}.

g(Phi) ->
    1 / (math:sqrt(1 + 3*Phi*Phi / (math:pi() * math:pi()))).


e(Mu, Muj, Phij) ->
    1 / (1 + math:exp(-g(Phij) * (Mu - Muj))).

%% The Opponents list ends up like the following for a given user
%% [{Muj, Phij, g(Phij), E(Mu, Muj, Phij), Sj}]
scale_opponents(Mu, Opponents) ->
    [begin
         {Muj, Phij} = scale(Rj, RDj),
         {Muj, Phij, g(Phij), e(Mu, Muj, Phij), Sj}
     end || {Rj, RDj, Sj} <- Opponents].

%% Step 3
update_rating(Opponents) ->
    1 / (lists:sum([square(GPhij) * EMMP * (1 - EMMP)
                    || {_Muj, _Phij, GPhij, EMMP, _Score} <- Opponents])).

%% Step 4
compute_delta(V, Opponents) ->
    V * lists:sum([GPhij * (Sj - EMMP)
                   || {_Muj, _Phij, GPhij, EMMP, Sj} <- Opponents]).

%% Step 5
compute_volatility(Sigma, Phi, V, Delta) ->
    A = math:log(Sigma*Sigma),
    compute_volatility(Sigma, Phi, V, Delta, A, A).

compute_volatility(Sigma, Phi, V, Delta, A, Xn) ->
    EXn = math:exp(Xn),
    D = Phi*Phi + V + EXn,
    H1 = -(Xn - A) / square(?TAU)
         - 0.5*EXn / D
         + 0.5*EXn * square(Delta / D),
    H2 = -1/square(?TAU)
        - 0.5*EXn * (square(Phi) + V) / square(D)
        + 0.5*square(Delta) * EXn
                            * (square(Phi) + V - EXn)/(D*D*D),
    XNext = Xn - (H1 / H2),
    case abs(Xn - XNext) < ?EPSILON of
        true ->
            math:exp(XNext / 2);
        false ->
            compute_volatility(Sigma, Phi, V, Delta, A, XNext)
    end.

%% Step 6
phi_star(SigmaP, Phi) ->
    math:sqrt(square(Phi) + square(SigmaP)).

% Step 7
new_rating(PhiStar, Mu, V, Opponents) ->
    PhiP = 1 / math:sqrt(
                  (1 / square(PhiStar))
                 + (1 / V)),
    L = [GPhij * (Sj - EMMP)
         || {_Muj, _Phij, GPhij, EMMP, Sj} <- Opponents],
    MuP  = Mu + square(PhiP) * lists:sum(L),
    {MuP, PhiP}.

% Step 8
unscale(MuP, PhiP) ->
    RP = 173.7178*MuP + 1500,
    RDP = 173.7178*PhiP,
    {RP, RDP}.

rate(R, RD, Sigma, Opponents) ->
    {Mu, Phi} = scale(R, RD),
    ScaledOpponents = scale_opponents(Mu, Opponents),
    V = update_rating(ScaledOpponents),
    Delta = compute_delta(V, ScaledOpponents),
    SigmaP = compute_volatility(Sigma, Phi, V, Delta),
    PhiStar = phi_star(SigmaP, Phi),
    {MuP, PhiP} = new_rating(PhiStar, Mu, V, ScaledOpponents),
    {R1, RD1} = unscale(MuP, PhiP),
    {R1, RD1, SigmaP}.

-ifdef(EUNIT).

data() ->
    Player = {a, 1500, 200},
    Volatility = 0.06,
    Opponents = [{1400, 30,  1},
                 {1550, 100, 0},
                 {1700, 300, 0}],
    {Player, Volatility, Opponents}.

glicko_test() ->
    {{a, R, RD}, Sigma, Opponents} = data(),
    {Mu, Phi} = scale(R, RD),
    ?assertEqual({0.0, 1.1512924985234674}, {Mu, Phi}),
    ScaledOpponents = scale_opponents(Mu, Opponents),
    ?assertEqual(
       [{-0.5756462492617337,0.1726938747785201,
          0.9954980064506083,0.6394677305521533,1},

        {0.28782312463086684,0.5756462492617337,
         0.9531489778689763,0.4318423561076679,0},

        {1.1512924985234674,1.726938747785201,
         0.7242354780877526,0.30284072909521925,0}],
       ScaledOpponents),
    V = update_rating(ScaledOpponents),
    ?assertEqual(1.7789770897239976, V),
    Delta = compute_delta(V, ScaledOpponents),
    ?assertEqual(-0.4839332609836549, Delta),
    SigmaP = compute_volatility(Sigma, Phi, V, Delta),
    ?assertEqual(0.059995984400677826, SigmaP),
    PhiStar = phi_star(SigmaP, Phi),
    ?assertEqual(1.152854689586079, PhiStar),
    {MuP, PhiP} = new_rating(PhiStar, Mu, V, ScaledOpponents),
    ?assertEqual({-0.20694096667647613, 0.8721991881333078}, {MuP, PhiP}),
    {R1, RD1} = unscale(MuP, PhiP),
    ?assertEqual({1464.0506705390892,151.51652412430434}, {R1, RD1}).

-endif.
