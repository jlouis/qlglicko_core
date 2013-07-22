-module(qlg_db).

-export([
         declare_match/1,
         matches_to_fetch/1,
         player_stats/1,
         store_match/2
        ]).

%% External API
%% --------------------------------------------------
player_stats(Player) ->
    {ok, Entries} = player_rank(Player),
    {ok, Streaks} = player_match_streak(Player),
    {ok, [{entries, Entries},
          {streaks, Streaks}]}.

store_match(Id, Data) ->
    {ok, _, Entries} = equery(processing, "SELECT processing.store_match($1, $2)", [Id, Data]),
    {ok, Entries}.

declare_match(Id) ->
    {ok, _, Entries} =
        equery(processing, "SELECT processing.declare_match($1)", [Id]),
    {ok, Entries}.

matches_to_fetch(Limit) ->
    {ok, _, Matches} = equery(processing, "SELECT id FROM processing.matches_to_fetch LIMIT $1", [Limit]),
    {ok, Matches}.

%% Queries
%% --------------------------------------------------
player_rank(Player) ->
    {ok, _, Entries} =
        equery(web,
               "SELECT tournament, map, rank, rd FROM web.player_rankings "
               "WHERE player = $1 "
               "ORDER BY tournament ASC",
               [Player]),
    {ok, Entries}.

player_match_streak(Player) ->
    {ok, _, Entries} =
        equery(web,
               "SELECT map, res, played FROM web.player_match_streak "
               "WHERE name = $1 "
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
