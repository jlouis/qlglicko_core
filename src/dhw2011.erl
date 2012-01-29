-module(dhw2011).

-export([matches/0]).

matches() ->
%%% Group stage
    [{cypher, reload, 2},

     {cypher, demon, 2},

     {cypher, spartie, 2},

     {spartie, demon, 2},

     {reload, spartie, 1},
     {spartie, reload, 2},

     {demon, reload, 2}]
        ++
    [{cooller, poni, 2},
     {poni, cooller, 1},

     {cooller, twister, 2},

     {cooller, avek, 1},
     {avek, cooller, 2},

     {avek, twister, 2},

     {poni, avek, 1},
     {avek, poni, 2},

     {poni, twister, 2}]
        ++
    [{strenx, ventz, 2},

     {strenx, madix, 2},
     {madix, strenx, 1},

     {strenx, killsen, 1},
     {killsen, strenx, 2},

     {killsen, madix, 2},
     {madix, killsen, 1},

     {killsen, ventz, 2},

     {madix, ventz, 2},
     {ventz, madix, 1}]
        ++
    [{rapha, draven, 2},

     {rapha, matrox, 2},

     {rapha, toxjq, 2},

     {toxjq, matrox, 2},

     {toxjq, draven, 2},

     {draven, matrox, 2}]
        ++
%%% Brackets
    [{killsen, cooller, 2},
     {cooller, killsen, 1},

     {cypher, toxjq, 2},

     {avek, spartie, 2},

     {rapha, strenx, 2},
     {strenx, rapha, 1},

     {cypher, killsen, 2},

     {rapha, avek, 2},

     {cypher, rapha, 3},

     {avek, killsen, 1}
    ].

