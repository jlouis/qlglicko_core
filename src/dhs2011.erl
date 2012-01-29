-module(dhs2011).

-export([matches/0]).

matches() ->
    [{cypher, evil, 2},

     {noctis, garpy, 2},
     {garpy, noctis, 1},

     {cooller, czm, 2},
     {czm, cooller, 1},

     {spartie, demon, 2},

     {strenx, madix, 2},

     {dahang, amai, 2},

     {rapha, fazz, 2},

     {avek, zsx, 2},
     {zsx, avek, 1},

     {noctis, evil, 2},

     {cypher, garpy, 2},

     {spartie, czm, 2},

     {cooller, demon, 2},

     {dahang, madix, 2},

     {strenx, amai, 2},
     {amai, strenx, 1},

     {avek, fazz, 2},

     {rapha, zsx, 2},

     {cypher, noctis, 2},
     {noctis, cypher, 1},

     {garpy, evil, 2},

     {cooller, spartie, 1},
     {spartie, cooller, 2},

     {demon, czm, 2},

     {dahang, strenx, 2},

     {madix, amai, 2},
     {amai, madix, 1},

     {avek, rapha, 2},

     {fazz, zsx, 2},
     {zsx, fazz, 1},

     %% Playoffs
     {cypher, strenx, 3},

     {avek, cooller, 2},
     {cooller, avek, 3},

     {dahang, noctis, 2},
     {noctis, dahang, 3},

     {spartie, rapha, 2},
     {rapha, spartie, 3},

     {cypher, cooller, 3},
     {cooller, cypher, 1},

     {noctis, rapha, 1},
     {rapha, noctis, 3},

     {cypher, rapha, 3},
     {rapha, cypher, 1}].
