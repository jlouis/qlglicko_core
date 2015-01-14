-module(qlg_db).
-include("match.hrl").

-export([
	alive_check/1,
         declare_match/1,
         fetch_player/1,
         fetch_match/1,
         mark_analyzed/1,
         matches_to_fetch/1,
         matches_to_analyze/1,
         players_to_refresh/1,
         player_refreshable/1,
         player_stats/1,
         refresh_player/1,
         store_match/2,
         store_duel/2,
         store_player/1,
         tournament_stats/2,
         update_alive/1
        ]).

%% Match processing
-export([
	process_unavailable/0,
	process_unranked/0
]).

%% Queries
-export([
	duel_counts/0,
	player_rankings/2
]).

%% External API
%% --------------------------------------------------
player_stats(Player) ->
    {ok, Entries} = player_rank(Player),
    {ok, Streaks} = player_match_streak(Player),
    {ok, [{entries, Entries},
          {streaks, Streaks}]}.

tournament_stats(Tourney, Count) ->
    {ok, _, Entries} = equery(web,
                              "SELECT player, map, rank, rd, sigma "
                              "FROM web.tourney_ranking "
                              "WHERE tournament = $1 "
                              "  AND tourney = $2", [Count, Tourney]),
    {ok, Entries}.

player_rankings(Player, Map) ->
    {ok, _, Entries} = equery(web,
        "SELECT date, rank, rd, sigma "
        "FROM web.player_rankings "
        "WHERE lower(player) = $1 "
        "  AND map = $2 "
        "ORDER BY date DESC", [Player, Map]),
    case Entries of
      [] -> not_found;
      [_|_] -> {ok, Entries}
    end.

duel_counts() ->
     {ok, _, Entries} = equery(web,
     	"SELECT map, count(id) AS cnt "
     	"FROM web.duel_match GROUP BY map ORDER BY cnt DESC", []),
     Entries.

process_unavailable() ->
    {ok, _, [{Changed}]} = equery(processing, "SELECT processing.analyze_unavailable()", []),
    Changed.
    
process_unranked() ->
    {ok, _, [{Changed}]} = equery(processing, "SELECT processing.analyze_unranked()", []),
    Changed.

store_match(Id, Data) ->
    {ok, _, [{1}]} = equery(processing, "SELECT processing.store_match($1, $2)", [Id, Data]),
    ok.

store_duel(ID, #duel_match {
		played = Played,
		map = M,
		winner = W,
		winner_score = Ws,
		loser = L,
		loser_score = Ls }) ->
    {ok, _} = equery(processing, "DELETE FROM duel_match WHERE id = $1", [ID]),
    {ok, 1} = equery(processing,
    	"INSERT INTO duel_match "
    	"(id, played, map, winner, winner_score, loser, loser_score) "
    	"VALUES ($1, $2, $3, $4, $5, $6, $7)",
    	[ID, Played, M, W, Ws, L, Ls]),
    ok.

store_player(Name) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    {ok, 1} = equery(processing,
        "INSERT INTO player (id, name, lastupdate) "
        "VALUES ($1, $2, now() - '5 days' :: interval)",
        [UUID, Name]),
    ok.

alive_check(ID) ->
    case equery(processing,
    		"SELECT id FROM player "
    		"WHERE id = $1 AND last_alive_check < (now() - '1 month' :: interval)", [ID]) of
        {ok, _, []} -> false;
        {ok, _, [_]} -> true
    end.

refresh_player(ID) ->
   {ok, 1} = equery(processing,
       "UPDATE player SET lastupdate = now() WHERE id = $1", [ID]),
   ok.

fetch_player(Name) ->
    case equery(web, "SELECT id,name FROM player WHERE name = $1", [Name]) of
        {ok, _, [Result]} -> {ok, Result};
        {ok, _, []} -> not_found
    end.

fetch_match(ID) ->
    {ok, _, [{Content}]} = equery(
    	processing,
    	"SELECT content FROM core.raw_match WHERE id = $1", [ID]),
    {ok, Content}.

update_alive(ID) ->
    {ok, 1} = equery(processing,
    	"UPDATE player SET last_alive_check = now() WHERE id = $1",
    	[ID]),
    ok.

mark_analyzed(ID) ->
    {ok, 1} = equery(processing,
    	"UPDATE core.raw_match SET analyzed = true WHERE id = $1", [ID]),
    ok.

declare_match(Id) ->
    {ok, _, Entries} =
        equery(processing, "SELECT processing.declare_match($1)", [Id]),
    {ok, Entries}.

matches_to_fetch(Limit) ->
    {ok, _, Matches} = equery(processing, "SELECT id FROM processing.matches_to_fetch LIMIT $1", [Limit]),
    {ok, Matches}.

matches_to_analyze(Limit) ->
    {ok, _, Matches} = equery(processing,
                              "SELECT id FROM processing.matches_to_analyze "
                              "LIMIT $1", [Limit]),
    {ok, Matches}.

players_to_refresh(Limit) ->
    {ok, _, Players} = equery(processing,
                              "SELECT id, name, age_days "
                              "FROM processing.players_to_refresh "
                              "ORDER BY age_days DESC "
                              "LIMIT $1", [Limit]),
    {ok, Players}.

player_refreshable(P) ->
    case equery(processing,
                "SELECT id, lastupdate "
                "FROM processing.players_to_refresh "
                "WHERE id = $1", [P]) of
        {ok, _, []} -> false;
        {ok, _, [_|_]} -> true
    end.

%% Queries
%% --------------------------------------------------
player_rank(Player) ->
    {ok, _, Entries} =
        equery(web,
               "SELECT tournament, map, rank, rd FROM web.player_rankings "
               "WHERE lower(player) = lower($1) "
               "ORDER BY tournament ASC",
               [Player]),
    {ok, Entries}.

player_match_streak(Player) ->
    {ok, _, Entries} =
        equery(web,
               "SELECT map, res, played FROM web.player_match_streak "
               "WHERE lower(name) = lower($1) "
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
