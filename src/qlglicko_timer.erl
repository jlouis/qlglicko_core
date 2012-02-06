-module(qlglicko_timer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([fetch_player/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

fetch_player(Name) ->
    gen_server:cast(?MODULE, {fetch_player, Name}).

%%%===================================================================

%% @private
init([]) ->
    erlang:send_after(100, self(), refill),
    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({fetch_player, Name}, State) ->
    qlg_fetch_player_pool:fetch_player(Name),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(refill, State) ->
    refill_players(),
    refill_matches(),
    erlang:send_after(60*1000, self(), refill),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

refill_players() ->
    {ok, _, Players} =
        qlg_pgsql_srv:players_to_refresh(),
    lager:debug("Submitting ~B player fetch jobs", [length(Players)]),
    [qlg_fetch_player_pool:fetch_player(Id, binary_to_list(Name))
     || {Id, Name} <- Players],
    ok.

refill_matches() ->
    {ok, _, Matches} =
        qlg_pgsql_srv:matches_to_fetch(),
    lager:debug("Submitting ~B match fetch jobs", [length(Matches)]),
    [qlg_fetch_match_pool:fetch_match(M) || {M} <- Matches],
    ok.
