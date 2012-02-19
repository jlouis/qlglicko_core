%%% @author Jesper Louis Andersen <jlouis@tiefling.local>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc Manage a generic server connection
%%% @end
%%% Created : 29 Jan 2012 by Jesper Louis Andersen <jlouis@tiefling.local>
-module(qlg_pgsql_srv).

-include("match.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([store_match/2,
         db_connect/0,
         select_player/1,
         players_to_refresh/0,
         matches_to_fetch/0,
         tournament_mark_ranked/1,
         matches_to_analyze/0,
         mark_analyzed/1,
         fetch_player_name/1,
         fetch_wins/2, fetch_wins/3,
         fetch_losses/2, fetch_losses/3,
         fetch_player_rating/1, fetch_player_rating/2,
         fetch_match/1,
         store_player_ranking/2,
         refresh_player/1,
         should_match_be_updated/1,
         should_player_be_refreshed/1,
         players_in_tournament/1,
         mk_player/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { conn }).

%%%===================================================================

%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

db_connect() ->
    {Host, Name, PW, DB} =
        gproc:get_env(l, qlglicko, postgres, [app_env, error]),
    pgsql:connect(Host, Name, PW, [{database, DB}]).

call(Msg) ->
    gen_server:call(?MODULE, Msg, 60000).

fetch_match(Id) ->
    gen_server:call(?MODULE, {fetch_match, Id}).

mk_player(Name) ->
    gen_server:call(?MODULE, {mk_player, Name}).

select_player(Name) ->
    gen_server:call(?MODULE, {select_player, Name}).

matches_to_analyze() ->
    gen_server:call(?MODULE, matches_to_analyze).

mark_analyzed(Id) ->
    gen_server:call(?MODULE, {mark_analyzed, Id}).

players_to_refresh() ->
    gen_server:call(?MODULE, players_to_refresh).

should_match_be_updated(Id) ->
    call({should_match_be_updated, Id}).

should_player_be_refreshed(Id) ->
    call({should_player_be_refreshed, Id}).

store_player_ranking(T, PI) ->
    call({store_player_ranking, T, PI}).

matches_to_fetch() ->
    gen_server:call(?MODULE, matches_to_fetch).

refresh_player(Id) ->
    gen_server:call(?MODULE, {refresh_player, Id}).

fetch_player_name(Id) ->
    gen_server:call(?MODULE, {fetch_player_name, Id}).

fetch_player_rating(C, P) ->
    ex_fetch_player_rating(C, P).

fetch_player_rating(P) ->
    gen_server:call(?MODULE, {fetch_player_rating, P}).

fetch_wins(C, P, T) ->
    ex_fetch_wins(C, P, T).

fetch_wins(P, T) ->
    gen_server:call(?MODULE, {fetch_wins, P, T}).

fetch_losses(C, P, T) ->
    ex_fetch_losses(C, P, T).

tournament_mark_ranked(T) ->
    call({tournament_mark_ranked, T}).

fetch_losses(P, T) ->
    gen_server:call(?MODULE, {fetch_losses, P, T}).

store_match(Id, Blob) when Blob == null;
                           is_binary(Blob) ->
    gen_server:call(?MODULE, {store_match, Id, Blob});
store_match(Id, #duel_match{} = DM) ->
    gen_server:call(?MODULE, {store_duel_match, Id, DM}).

players_in_tournament(T) ->
    gen_server:call(?MODULE, {players_in_tournament, T}).

%%%===================================================================

%% @private
init([]) ->
    {ok, C} = db_connect(),
    {ok, #state{ conn = C}}.

%% @private
handle_call({players_in_tournament, T}, _From,
            #state { conn = C } = State) ->
    Reply = ex_players_in_tournament(C, T),
    {reply, Reply, State};
handle_call({should_player_be_refreshed, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_should_player_be_refreshed(C, Id),
    {reply, Reply, State};
handle_call({should_match_be_updated, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_should_match_be_updated(C, Id),
    {reply, Reply, State};
handle_call({fetch_match, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_fetch_match(C, Id),
    {reply, Reply, State};
handle_call({fetch_player_name, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_fetch_player_name(C, Id),
    {reply, Reply, State};
handle_call(matches_to_fetch, _From, #state { conn = C } = State) ->
    Reply = ex_matches_to_fetch(C),
    {reply, Reply, State};
handle_call({fetch_wins, P, T}, _From, #state {conn = C} = State) ->
    Reply = ex_fetch_wins(C, P, T),
    {reply, Reply, State};
handle_call({fetch_losses, P, T}, _From, #state {conn = C} = State) ->
    Reply = ex_fetch_losses(C, P, T),
    {reply, Reply, State};
handle_call({store_player_ranking, T, PI},
            _From, #state { conn = C } = State) ->
    Reply = ex_store_player_ranking(C, T, PI),
    {reply, Reply, State};
handle_call(matches_to_analyze, _From, #state { conn = C } = State) ->
    Reply = ex_matches_to_analyze(C),
    {reply, Reply, State};
handle_call(players_to_refresh, _From, #state { conn = C } = State) ->
    Reply = ex_players_to_refresh(C),
    {reply, Reply, State};
handle_call({fetch_player_rating, P}, _From, #state {conn = C} = State) ->
    Reply = ex_fetch_player_rating(C, P),
    {reply, Reply, State};
handle_call({select_player, Name}, _From, #state { conn = C } = State) ->
    Reply = ex_select_player(C, Name),
    {reply, Reply, State};
handle_call({mark_analyzed, Id}, _From, #state { conn = C } = State) ->
    Reply = ex_mark_analyzed(C, Id),
    {reply, Reply, State};
handle_call({mk_player, Name}, _From, #state { conn = C } = State) ->
    Reply = ex_store_player(C, Name),
    {reply, Reply, State};
handle_call({refresh_player, Id}, _From, #state { conn = C } = State) ->
    Reply = ex_refresh_player(C, Id),
    {reply, Reply, State};
handle_call({tournament_mark_ranked, Id},
            _From, #state { conn = C } = State) ->
    Reply = ex_mark_tournament_ranked(C, Id),
    {reply, Reply, State};
handle_call({store_match, Id, Blob}, _From, #state { conn = C } = State) ->
    Reply = ex_store_match(C, Id, Blob),
    {reply, Reply, State};
handle_call({store_duel_match, Id, MatchRec}, _From,
            #state { conn = C} = State) ->
    Reply = ex_store_duel_match(C, Id, MatchRec),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
ex_players_in_tournament(C, T) ->
    pgsql:equery(
      C,
      "SELECT player FROM tournament_players WHERE tournament = $1",
      [T]).

ex_fetch_player_rating(C, P) ->
    pgsql:equery(
      C,
      "SELECT id, r, rd, sigma "
      "FROM player_ratings "
      "WHERE id = $1", [P]).

ex_fetch_match(C, Id) ->
    {ok, _, [{Content}]} = pgsql:equery(
                             C,
                             "SELECT content FROM raw_match WHERE id = $1",
                             [Id]),
    {ok, Content}.

ex_matches_to_analyze(C) ->
    pgsql:equery(
      C,
      "SELECT id FROM matches_to_analyze LIMIT 3000").

ex_fetch_player_name(C, Id) ->
    {ok, _, [{Pname}]} = pgsql:equery(C,
                                      "SELECT name FROM player where id = $1",
                                      [Id]),
    binary_to_list(Pname).

ex_fetch_wins(C, P, T) ->
    {ok, _, Matches} = pgsql:equery(C,
                                    "SELECT loser, lr, lrd "
                                    "FROM duel_match_ratings, tournament t "
                                    "WHERE winner = $1 "
                                    "AND t.id = $2 "
                                    "AND played BETWEEN "
                                    " t.t_from AND t.t_to", [P, T]),
    [{Rj, RDj, 1} || {_, Rj, RDj} <- Matches].

ex_fetch_losses(C, P, T) ->
    {ok, _, Matches} = pgsql:equery(C,
                                    "SELECT winner, wr, wrd "
                                    "FROM duel_match_ratings, tournament t "
                                    "WHERE loser = $1 "
                                    "AND t.id = $2 "
                                    "AND played BETWEEN "
                                    " t.t_from AND t.t_to", [P, T]),
    [{Rj, RDj, 0} || {_, Rj, RDj} <- Matches].

ex_mark_tournament_ranked(C, T) ->
    pgsql:equery(C,
                 "UPDATE tournament SET done = true WHERE id = $1", [T]).

ex_store_player_ranking(C, T, {Id, R, RD, Sigma}) ->
    pgsql:equery(C,
                 "INSERT INTO tournament_result (id, player_id, r, rd, sigma)"
                 "VALUES ($1, $2, $3, $4, $5)", [T, Id, R, RD, Sigma]).

ex_matches_to_fetch(C) ->
    pgsql:equery(C, "SELECT id FROM matches_to_refresh LIMIT 66").

ex_players_to_refresh(C) ->
    pgsql:equery(C, "SELECT id,name FROM players_to_update LIMIT 66").

ex_select_player(C, Name) ->
    pgsql:equery(C, "SELECT id,name FROM player WHERE name = $1",
                 [Name]).

ex_mark_analyzed(C, Id) ->
    {ok, 1} = pgsql:equery(
                C,
                "UPDATE raw_match SET analyzed = true WHERE id = $1", [Id]).

ex_refresh_player(C, Id) ->
    {ok, 1} = pgsql:equery(C,
                           "UPDATE player SET lastupdate = now()"
                           "WHERE id = $1", [Id]).

ex_store_player(C, Name) ->
    {ok, 1} = pgsql:equery(C,
                           "INSERT INTO player (name, lastupdate)"
                           "VALUES ($1, now() - '5 days' :: interval)",
                           [Name]),
    ok.

knows_match(C, Id) ->
    case pgsql:equery(C, "SELECT id FROM raw_match WHERE id = $1", [Id]) of
        {ok, _, []} ->
            false;
        {ok, _, [_|_]} ->
            true
    end.

ex_store_match(C, Id, null) ->
    case knows_match(C, Id) of
        true ->
            {ok, 0};
        false ->
            {ok, 1} = pgsql:equery(C, "INSERT INTO raw_match (id, content)"
                                   "VALUES ($1, NULL)", [Id])
    end;
ex_store_match(C, Id, B) when is_binary(B) ->
    {ok, 1} = pgsql:equery(C, "UPDATE raw_match SET content = $2 WHERE id = $1",
                           [Id, B]).

ex_should_player_be_refreshed(C, Name) ->
    case pgsql:equery(
           C,
           "SELECT id FROM players_to_update WHERE id = $1", [Name]) of
        {ok, _, []} ->
            false;
        {ok, _, [_|_]} ->
            true
    end.

ex_should_match_be_updated(C, Id) ->
    case pgsql:equery(
           C,
           "SELECT id FROM matches_to_refresh WHERE id = $1", [Id]) of
        {ok, _, []} ->
            false;
        {ok, _, [_|_]} ->
            true
    end.

ex_store_duel_match(C, Id,
                    #duel_match {
                         played = Played,
                         winner = Winner,
                         winner_score = WinnerS,
                         loser = Loser,
                         loser_score = LoserS}) ->
    {ok, _} = pgsql:equery(C, "DELETE FROM duel_match WHERE id = $1", [Id]),
    {ok, 1} = pgsql:equery(
                C,
                "INSERT INTO duel_match "
                "(id, played, winner, winner_score, loser, loser_score) "
                " VALUES ($1, $2, $3, $4, $5, $6)",
                [Id, Played, Winner, WinnerS, Loser, LoserS]).
