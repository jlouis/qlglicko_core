-module(iem5).

-export([matches/0]).

matches() ->
    transform(raw()).

transform([]) ->
    [];
transform([{_, _, _} = T | Next]) ->
    [T | transform(Next)];
transform([{Pa, Na, Pb, Nb} | Next]) ->
    wins(Na, Pa, Pb) ++ wins(Nb, Pb, Pa) ++ transform(Next).

wins(0, _, _) -> [];
wins(K, Pa, Pb) when K > 0 ->
    [{Pa, Pb, K}].

raw() ->
    [{rapha, czm, 2},

     {cypher, cooller, 1},
     {cooller, cypher, 2},

     {rapha, cooller, 1},
     {cooller, rapha, 2},

     {dandaking, spartie, 2},
     {spartie, dandaking, 1},

     {cypher, spartie, 1},
     {spartie, cypher, 2},

     {dandaking, czm, 2},
     {czm, dandaking, 1},

     {rapha, dandaking, 2},

     {cooller, spartie, 1},
     {spartie, cooller, 2},

     {cypher, czm, 2},

     {rapha, spartie, 1},
     {spartie, rapha, 2},

     {cypher, dandaking, 2},

     {cooller, czm, 2},

     {cooller, dandaking, 2},

     {czm, spartie, 1},
     {spartie, czm, 2},

     {rapha, 2, cypher, 1},

     {killsen, 1, dahang, 2},
     {strenx, 2, avek, 0},
     {killsen, 0, avek, 2},

     {voo, 0, fazz, 2},
     {strenx, 2, fazz, 1},
     {voo, 0, dahang, 2},

     {killsen, 2, voo, 0},
     {strenx, 1, dahang, 2},
     {avek, 2, fazz, 1},

     {strenx, 2, voo, 0},
     {avek, 2, dahang, 1},
     {killsen, 0, fazz, 2},

     {killsen, 0, strenx, 2},
     {voo, 0, avek, 2},
     {dahang, 2, fazz, 1},

     %% Playoffs
     {dahang, 1, rapha, 3},
     {cooller, 3, avek, 0},

     {spartie, 0, rapha, 3},
     {strenx, 1, cooller, 3},

     {spartie, 0, strenx, 3},
     {rapha, 3, cooller, 0}].
