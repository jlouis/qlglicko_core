-module(quakecon2011).

-export([matches/0]).

matches() ->
%%% Group stage
    %% Group A
    [{cooller, zero4, 2},
     {zero4, cooller, 1},

     {ventz, draven, 1},
     {draven, ventz, 2},

     {cooller, draven, 2},

     {ventz, zero4, 1},
     {zero4, ventz, 2},

     {cooller, ventz, 2},

     {zero4, draven, 2},
     %% Group B
     {noctis, vampire, 2},

     {czm, linkin, 2},

     {noctis, czm, 2},

     {vampire, linkin, 2},
     {linkin, vampire, 1},

     {noctis, vampire, 2},

     {czm, vampire, 2},
     %% Group C
     {rapha, clock, 2},

     {chance, zlr, 2},

     {rapha, zlr, 2},

     {chance, clock, 2},

     {rapha, chance, 2},

     {clock, zlr, 2},
     %% Group D
     {avek, dahang, 1},
     {dahang, avek, 2},

     {chaoticz, bichito, 2},

     {avek, chaoticz, 2},

     {dahang, bichito, 2},

     {avek, bichito, 2},

     {dahang, chaoticz, 2},
     %% Group E
     {strenx, krysa, 2},

     {toxjq, dkt, 2},

     {strenx, dkt, 2},

     {toxjq, krysa, 2},

     {toxjq, strenx, 2},

     {dkt, krysa, 2},
     %% Group F
     {cypher, scoot, 2},

     {spartie, ziel, 2},

     {cypher, ziel, 2},

     {spartie, scoot, 2},

     {cypher, spartie, 1},
     {spartie, cypher, 2},

     {scoot, ziel, 2}] ++
%%% Playoffs
     %% 1/8
    [{rapha, bichito, 2},

     {avek, zero4, 2},

     {spartie, czm, 1},
     {czm, spartie, 2},

     {dahang, dkt, 2},

     {toxjq, draven, 2},

     {cooller, chance, 2},

     {strenx, cypher, 2},

     {noctis, ventz, 2},
     %% 1/4
     {rapha, avek, 2},

     {czm, dahang, 2},
     {dahang, czm, 1},

     {cooller, toxjq, 2},

     {strenx, noctis, 2},
     %% 1/2
     {rapha, czm, 2},

     {cooller, strenx, 1},
     {strenx, cooller, 2},
     %% Bronze match
     {czm, cooller, 2},
     {cooller, czm, 3},
     %% Final
     {rapha, strenx, 3},
     {strenx, rapha, 1}].
