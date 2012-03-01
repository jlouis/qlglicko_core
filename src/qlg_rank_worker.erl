%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <jlouis@tiefling.local>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2012 by Jesper Louis Andersen <jlouis@tiefling.local>
%%%-------------------------------------------------------------------
-module(qlg_rank_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, run/2, done/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { info }).

%%%===================================================================

%% @doc
%% Starts the server
%% @end
start_link(Info) ->
    gen_server:start_link(?MODULE, [Info], []).

run(Pid, Players) ->
    gen_server:cast(Pid, {run, Players}).

done(Pid) ->
    gen_server:call(Pid, done, infinity).

%%%===================================================================

%% @private
init([Info]) ->
    {ok, #state { info = Info }}.

%% @private
handle_call(done, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({run, Ps}, State) ->
    rank_chunk(Ps),
    {noreply, State};
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
rank_chunk(Players) ->
    lager:debug("Requesting jobs to run a ranking job for us"),
    jobs:run(qlrank,
             fun() ->
                     lager:debug("Running ranking job"),
                     _ = [rank_player(P) || P <- Players],
                     ok
             end).

rank_player(P) ->
    {P, R, RD1, Sigma} = rank1(P),
    lager:debug("Ranked player ~p (~p, ~p, ~p)", [P, R, RD1, Sigma]),
    store_player_rating(P, R, RD1, Sigma),
    ok.

player_ranking(P) ->
    case ets:lookup(qlg_player_ratings, P) of
        [] ->
            {P, 1500.0, 350.0, 0.06};
        [Rating] ->
            Rating
    end.
    
rank1(Player) ->
    lager:debug("Fetching player rating"),
    {Player, R, RD, Sigma} = player_ranking(Player),
    Wins = [begin
                {_P, R, RD, _Sigma} = player_ranking(Opp),
                {R, RD, 1}
            end || {_, Opp} <- ets:lookup(qlg_matches, {Player, w})],
    Losses = [begin
                  {_P, R, RD, _Sigma} = player_ranking(Opp),
                  {R, RD, 0}
              end || {_, Opp} <- ets:lookup(qlg_matches, {Player, l})],
    case Wins ++ Losses of
        [] ->
            RD1 = glicko2:phi_star(RD, Sigma),
            {Player, R, RD1, Sigma};
        Opponents ->
            {R1, RD1, Sigma1} =
                glicko2:rate(R, RD, Sigma, Opponents),
            {Player, R1, RD1, Sigma1}
    end.

store_player_rating(P, R, RD, Sigma) ->
    ets:insert(qlg_rank, {P, R, RD, Sigma}).


