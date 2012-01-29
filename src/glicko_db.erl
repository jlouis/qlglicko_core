-module(glicko_db).

-export([create_db/0,
         csv/1,
         add_new_players/1,
         del_db/0,
         players/0,
         lookup/1,
         update_rating/1]).

-define(TAB, ql_player_db).

create_db() ->
    ets:new(?TAB, [named_table, public]).

add_new_players(Players) ->
    [insert_player(P) || P <- Players].

insert_player(P) ->
    case lookup(P) of
        [] ->
            ets:insert(?TAB, {P, 1500.0, 350.0, 0.06}),
            ok;
        [_] ->
            ok
    end.

del_db() ->
    ets:delete(?TAB).

lookup(P) ->
    ets:lookup(?TAB, P).

players() ->
    ets:match(?TAB, {'$1', '_', '_', '_'}).

update_rating(Players) when is_list(Players) ->
    ets:insert(?TAB, Players).

csv(Fname) ->
    Ratings = ets:match_object(?TAB, '$1'),
    IoData = ["Player,R,RD,Sigma", $\n,
              [[mk_rating(R), $\n] || R <- Ratings]],
    file:write_file(Fname, IoData).

mk_rating({Player, R, RD, Volatility}) ->
    [atom_to_list(Player), $,, float_to_list(R), $,,
     float_to_list(RD), $,, float_to_list(Volatility)].
