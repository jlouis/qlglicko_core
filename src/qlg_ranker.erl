%%%-------------------------------------------------------------------
%%% @author Jesper Louis andersen <jlouis@myrddraal>
%%% @copyright (C) 2012, Jesper Louis andersen
%%% @doc Ranker for matches
%%%
%%% @end
%%% Created : 19 Feb 2012 by Jesper Louis andersen <jlouis@myrddraal>
%%%-------------------------------------------------------------------
-module(qlg_ranker).

-behaviour(gen_server).

%% API
-export([start_link/1,
         rank/1, rank/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { dispatch_info }).

%%%===================================================================

%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Info) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Info], []).

rank(Id) ->
    rank(Id, []).

rank(Id, Opts) ->
    gen_server:call(?SERVER, {rank, Id, Opts}, 120*1000).

%%%===================================================================

%% @private
init([Info]) ->
    {ok, #state{ dispatch_info = Info}}.

%% @private
handle_call({rank, Id, Opts}, _From, #state { dispatch_info = Info} = State) ->
    Reply = qlg_rank:rank(Id, Info, Opts),
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
