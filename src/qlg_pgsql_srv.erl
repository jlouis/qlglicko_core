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
         select_player/1,
         players_to_refresh/0,
         matches_to_fetch/0,
         refresh_player/1,
         should_match_be_updated/1,
         should_player_be_refreshed/1,
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

call(Msg) ->
    gen_server:call(?MODULE, Msg, 60000).

mk_player(Name) ->
    gen_server:call(?MODULE, {mk_player, Name}).

select_player(Name) ->
    gen_server:call(?MODULE, {select_player, Name}).

players_to_refresh() ->
    gen_server:call(?MODULE, players_to_refresh).

should_match_be_updated(Id) ->
    call({should_match_be_updated, Id}).

should_player_be_refreshed(Id) ->
    call({should_player_be_refreshed, Id}).

matches_to_fetch() ->
    gen_server:call(?MODULE, matches_to_fetch).

refresh_player(Name) ->
    gen_server:call(?MODULE, {refresh_player, Name}).

store_match(Id, Blob) when Blob == null;
                           is_binary(Blob) ->
    gen_server:call(?MODULE, {store_match, Id, Blob});
store_match(Id, #duel_match{} = DM) ->
    gen_server:call(?MODULE, {store_duel_match, Id, DM}).


%%%===================================================================

%% @private
init([]) ->
    {Host, Name, PW, DB} =
        gproc:get_env(l, qlglicko, postgres, [app_env, error]),
    {ok, C} = pgsql:connect(Host, Name, PW, [{database, DB}]),
    {ok, #state{ conn = C}}.

%% @private
handle_call({should_player_be_refreshed, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_should_player_be_refreshed(C, Id),
    {reply, Reply, State};
handle_call({should_match_be_updated, Id}, _From,
            #state { conn = C } = State) ->
    Reply = ex_should_match_be_updated(C, Id),
    {reply, Reply, State};
handle_call(matches_to_fetch, _From, #state { conn = C } = State) ->
    Reply = ex_matches_to_fetch(C),
    {reply, Reply, State};
handle_call(players_to_refresh, _From, #state { conn = C } = State) ->
    Reply = ex_players_to_refresh(C),
    {reply, Reply, State};
handle_call({select_player, Name}, _From, #state { conn = C } = State) ->
    Reply = ex_select_player(C, Name),
    {reply, Reply, State};
handle_call({mk_player, Name}, _From, #state { conn = C } = State) ->
    Reply = ex_store_player(C, Name),
    {reply, Reply, State};
handle_call({refresh_player, Name}, _From, #state { conn = C } = State) ->
    Reply = ex_refresh_player(C, Name),
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
ex_matches_to_fetch(C) ->
    pgsql:equery(C, "SELECT id FROM matches_to_refresh LIMIT 300").

ex_players_to_refresh(C) ->
    pgsql:equery(C, "SELECT name FROM players_to_update LIMIT 300").

ex_select_player(C, Name) ->
    pgsql:equery(C, "SELECT name FROM player WHERE name = $1",
                 [Name]).

ex_refresh_player(C, Name) ->
    {ok, 1} = pgsql:equery(C,
                           "UPDATE player SET lastupdate = now()"
                           "WHERE name = $1", [Name]).

ex_store_player(C, Name) ->
    {ok, 1} = pgsql:equery(C,
                           "INSERT INTO player (name, lastupdate)"
                           "VALUES ($1, '1970-01-01')",
                           [Name]).

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
