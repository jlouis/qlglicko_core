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
         refresh_player/1,
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

mk_player(Name) ->
    gen_server:call(?MODULE, {mk_player, Name}).

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
    %% @todo lift these out
    {ok, C} = pgsql:connect("localhost", "jlouis", "", [{datebase, "jlouis"}]),
    {ok, #state{ conn = C}}.

%% @private
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

ex_refresh_player(C, Name) ->
    {ok, 1} = pgsql:equery(C,
                           "UPDATE player SET lastupdate = now()"
                           "WHERE name = $1", [Name]).

ex_store_player(C, Name) ->
    {ok, 1} = pgsql:equery(C,
                           "INSERT INTO player (name, lastupdate)"
                           "VALUES ($1, '1970-01-01')",
                           Name).

ex_store_match(C, Id, null) ->
    {ok, 1} = pgsql:equery(C, "INSERT INTO raw_match (id, content)"
                              "VALUES ($1, NULL)", [Id]);
ex_store_match(C, Id, B) when is_binary(B) ->
    {ok, 1} = pgsql:equery(C, "INSERT INTO raw_match (id, content)"
                              "VALUES ($1, $2)", [Id, B]).

ex_store_duel_match(C, Id,
                    #duel_match {
                         played = Played,
                         winner = Winner,
                         winner_score = WinnerS,
                         loser = Loser,
                         loser_score = LoserS}) ->
    {ok, 1} = pgsql:equery(C,
                           "INSERT INTO duel_match"
                           "(id, played, winner, winner_score"
                           " loser, loser_score) VALUES "
                           "($1, $2, $3, $4, $5, $6)",
                           [Id, Played, Winner, WinnerS, Loser, LoserS]).
