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
         add_to_hall_of_fame/2,
         alive_check/1,
         all_tournaments/0,
         all_players/0,
         bump_alive/1,
         tournament_matches/1,
         select_player/1,
         mark_analyzed/1,
         fetch_match/1,
         refresh_player/1,
         remove_active_player/1,
         mk_player/1
        ]).

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

call(Msg) ->
    gen_server:call(?MODULE, Msg, timer:minutes(2)).

all_players() ->
    call(all_players).

all_tournaments() ->
    call(all_tournaments).

fetch_match(Id) ->
    call({fetch_match, Id}).

mk_player(Name) ->
    call({mk_player, Name}).

select_player(Name) ->
    call({select_player, Name}).

mark_analyzed(Id) ->
    call({mark_analyzed, Id}).

tournament_matches(T) ->
    call({tournament_matches, T}).

refresh_player(Id) ->
    call({refresh_player, Id}).

alive_check(Id) ->
    call({alive_check, Id}).
    
bump_alive(Id) ->
    call({bump_alive, Id}).

store_match(Id, #duel_match{} = DM) ->
    call({store_duel_match, Id, DM}).

add_to_hall_of_fame(Id, Name) ->
    call({add_to_hall_of_fame, Id, Name}).

remove_active_player(Id) ->
    call({remove_active, Id}).

%%%===================================================================

%% @private
init([]) ->
    {ok, C} = db_connect(),
    {ok, #state{ conn = C}}.

%% @private
handle_call({fetch_match, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_fetch_match(C, Id),
    {reply, Reply, State};
handle_call(all_players, _From,
            #state { conn = C } = State) ->
    Reply = ex_all_players(C),
    {reply, Reply, State};
handle_call(all_tournaments, _From,
            #state { conn = C } = State) ->
    Reply = ex_all_tournaments(C),
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
handle_call({tournament_matches, T}, _From, #state { conn = C } = State) ->
    Reply = ex_tournament_matches(C, T),
    {reply, Reply, State};
handle_call({store_duel_match, Id, MatchRec}, _From,
            #state { conn = C} = State) ->
    Reply = ex_store_duel_match(C, Id, MatchRec),
    {reply, Reply, State};
handle_call({remove_active, Id}, _From, #state { conn = C } = State) ->
    Reply = ex_remove_active_player(C, Id),
    {reply, Reply, State};
handle_call({add_to_hall_of_fame, Id, Name}, _From, #state { conn = C } = State) ->
    Reply = ex_add_to_hall_of_fame(C, Id, Name),
    {reply, Reply, State};
handle_call({alive_check, Id}, _From, #state { conn = C } = State) ->
    Reply = ex_alive_check(C, Id),
    {reply, Reply, State};
handle_call({bump_alive, Id}, _From, #state { conn = C } = State) ->
    Reply = ex_bump_alive(C, Id),
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
ex_fetch_match(C, Id) ->
    {ok, _, [{Content}]} = pgsql:equery(
                             C,
                             "SELECT content FROM raw_match WHERE id = $1",
                             [Id]),
    {ok, Content}.

ex_all_tournaments(C) ->
    {ok, _, Tournaments} =
        pgsql:equery(
          C,
          "SELECT id FROM tournament "
          "ORDER BY t_from ASC"),
    {ok, [T || {T} <- Tournaments]}.

ex_all_players(C) ->
    {ok, _, Players} =
        pgsql:equery(
          C,
          "SELECT id, name FROM player"),
    {ok, Players}.

ex_tournament_matches(C, T) ->
    {ok, _, Matches} =
        pgsql:equery(
          C,
          "SELECT winner, loser, map FROM duel_match dm, tournament t "
          "WHERE t.id = $1 AND played BETWEEN t.t_from and t.t_to",
          [T]),
    Matches.

ex_select_player(C, Name) ->
    pgsql:equery(C, "SELECT id,name FROM player WHERE name = $1",
                 [Name]).

ex_mark_analyzed(C, Id) ->
    {ok, 1} = pgsql:equery(
                C,
                "UPDATE raw_match SET analyzed = true WHERE id = $1", [Id]).

ex_alive_check(C, Id) ->
    case pgsql:equery(C,
    		"SELECT id FROM player "
    		"WHERE id = $1 AND last_alive_check < (now() - '1 month' :: interval)", [Id]) of
    	{ok, _, []} -> false;
    	{ok, _, [_]} -> true
    end.

ex_bump_alive(C, Id) ->
    {ok, 1} = pgsql:equery(C,
    	"UPDATE player SET last_alive_check = now() "
    	"WHERE id = $1", [Id]).

ex_refresh_player(C, Id) ->
    {ok, 1} = pgsql:equery(C,
                           "UPDATE player SET lastupdate = now() "
                           "WHERE id = $1", [Id]).

ex_store_player(C, Name) ->
    {ok, 1} = pgsql:equery(C,
                           "INSERT INTO player (name, lastupdate)"
                           "VALUES ($1, now() - '5 days' :: interval)",
                           [Name]),
    ok.

ex_remove_active_player(C, Id) ->
   {ok, 1} = pgsql:equery(C,
	"DELETE FROM player WHERE id = $1", [Id]).

ex_add_to_hall_of_fame(C, Id, Name) ->
    pgsql:equery(C,
    	"DELETE FROM hall_of_fame WHERE id = $1 AND name = $2", [Id, Name]),
    pgsql:equery(C,
         "INSERT INTO hall_of_fame (id, name, entry) VALUES ($1, $2, now())", [Id, Name]).

ex_store_duel_match(C, Id,
                    #duel_match {
                         played = Played,
                         map = Map,
                         winner = Winner,
                         winner_score = WinnerS,
                         loser = Loser,
                         loser_score = LoserS}) ->
    {ok, _} = pgsql:equery(C, "DELETE FROM duel_match WHERE id = $1", [Id]),
    {ok, 1} = pgsql:equery(
                C,
                "INSERT INTO duel_match "
                "(id, played, map, winner, winner_score, loser, loser_score) "
                " VALUES ($1, $2, $3, $4, $5, $6, $7)",
                [Id, Played, Map, Winner, WinnerS, Loser, LoserS]).

db_connect() ->
    {Host, Name, PW, DB} =
        gproc:get_env(l, qlglicko_core, postgres, [app_env, error]),
    pgsql:connect(Host, Name, PW, [{database, DB}]).

