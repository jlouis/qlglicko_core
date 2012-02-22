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
-export([start_link/1, run/3, done/1]).

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

run(Pid, T, Players) ->
    gen_server:cast(Pid, {run, T, Players}).

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
handle_cast({run, T, Ps}, #state { info = I } = State) ->
    rank_chunk(Ps, T, I),
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
rank_chunk(Players, Tournament, Info) ->
    lager:debug("Requesting jobs to run a ranking job for us"),
    jobs:run(qlrank,
             fun() ->
                     lager:debug("Running ranking job"),
                     case dispcount:checkout(Info) of
                         {ok, CheckinReference, C} ->
                             lager:debug(
                               "Get dispcount job, ranking ~B players",
                               [length(Players)]),
                             _ = [rank_player(P, C, Tournament)
                                  || P <- Players],
                             lager:debug("Checkin to dispcount again"),
                             dispcount:checkin(Info, CheckinReference, C)
                     end,
                     ok
             end).

rank_player(P, C, T) ->
    lager:debug("Ranking player ~p", [P]),
    {P, R, RD1, Sigma} = rank1(P, C, T),
    lager:debug("Ranked player ~p (~p, ~p, ~p)", [P, R, RD1, Sigma]),
    store_player_rating(P, R, RD1, Sigma, T),
    ok.

rank1(Player, C, T) ->
    lager:debug("Fetching player rating"),
    {Player, R, RD, Sigma} =
        case qlg_pgsql_srv:fetch_player_rating(C, Player) of
            {ok, _, []} ->
                {Player, 1500.0, 350.0, 0.06};
            {ok, _, [Rating]} ->
                Rating
        end,
    lager:debug("Fetching player wins"),
    Wins = qlg_pgsql_srv:fetch_wins(C, Player, T),
    lager:debug("Fetching player losses"),
    Losses = qlg_pgsql_srv:fetch_losses(C, Player, T),
    case Wins ++ Losses of
        [] ->
            RD1 = glicko2:phi_star(RD, Sigma),
            {Player, R, RD1, Sigma};
        Opponents ->
            {R1, RD1, Sigma1} =
                glicko2:rate(R, RD, Sigma, Opponents),
            {Player, R1, RD1, Sigma1}
    end.

store_player_rating(P, R, RD, Sigma, _T) ->
    ets:insert(qlg_rank, {P, R, RD, Sigma}).


