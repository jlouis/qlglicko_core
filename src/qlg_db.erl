-module(qlg_db).

-export([player_stats/1]).

%% External API
%% --------------------------------------------------
player_stats(Player) ->
    {ok, Entries} = player_rank(Player),
    {ok, Streaks} = player_match_streak(Player),
    {ok, [{entries, Entries},
          {streaks, Streaks}]}.

%% Queries
%% --------------------------------------------------
player_rank(Player) ->
    {ok, _, Entries} =
        equery(web,
               "SELECT tournament, map, rank, rd FROM web.player_rankings "
               "WHERE player ILIKE $1 "
               "ORDER BY tournament ASC",
               [Player]),
    {ok, Entries}.

player_match_streak(Player) ->
    {ok, _, Entries} =
        equery(web,
               "SELECT map, res, played FROM web.player_match_streak "
               "WHERE name ILIKE $1 "
               "ORDER BY played DESC "
               "LIMIT 150", [Player]),
    {ok, Entries}.

equery(Pool, Stmt, Params) ->
    poolboy:transaction(
      Pool,
      fun(Worker) ->
              qlg_pg_worker:equery(Worker, Stmt, Params)
      end).

%% squery(Pool, Stmt) ->
%%     poolboy:transaction(
%%       Pool,
%%       fun(Worker) ->
%%               qlg_pg_worker:squery(Worker, Stmt)
%%       end).
