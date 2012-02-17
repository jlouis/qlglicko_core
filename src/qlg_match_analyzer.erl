%%% @author Jesper Louis Andersen <jlouis@tiefling.local>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc Run an analyzer over a set of matches
%%% @end
%%% Created : 14 Feb 2012 by Jesper Louis Andersen <jlouis@tiefling.local>
%%%-------------------------------------------------------------------
-module(qlg_match_analyzer).

-include("match.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([analyze_matches/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================

%% @doc
%% Starts the server
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

analyze_matches(Ids) ->
    gen_server:cast(?MODULE, {analyze, Ids}).

%%%===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({analyze, Ids}, State) ->
    [analyze_duel_match(Id) || Id <- Ids],
    {noreply, State};
handle_cast(Msg, State) ->
    ok = lager:error("Unknown Msg: ~p", [Msg]),
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

analyze_duel_match(Id) ->
    {ok, Bin} = qlg_pgsql_srv:fetch_match(Id),
    JSON = binary_to_term(Bin),
    persist_match(Id, JSON),
    qlg_pgsql_srv:mark_analyzed(Id).

persist_match(Id, JSON) ->
    case proplists:get_value(<<"UNAVAILABLE">>, JSON) of
        1 ->
            ok;
        undefined ->
            persist_duel_match(Id, JSON)
    end.

persist_duel_match(Id, JSON) ->
    case {proplists:get_value(<<"GAME_TYPE">>, JSON),
          proplists:get_value(<<"RANKED">>, JSON)} of
        {<<"duel">>, <<"1">>} ->
            %% Duel game type. And ranked. Find the winner and the loser
            case proplists:get_value(<<"SCOREBOARD">>, JSON) of
                [P1, P2] ->
                    Played = proplists:get_value(<<"GAME_TIMESTAMP">>, JSON),
                    P1S = extract_scores(P1),
                    P2S = extract_scores(P2),
                    {ok, P1_Id} = add_new_player(P1S),
                    {ok, P2_Id} = add_new_player(P2S),
                    case mk_match(decode_timestamp(Played),
                                 {P1_Id, P1S},
                                  {P2_Id, P2S}) of
                        {ok, M} ->
                            qlg_pgsql_srv:store_match(Id, M);
                        {error, Reason} ->
                            lager:info("Not analyzing match ~p: ~p",
                                       [Id, Reason])
                    end,
                    ok;
                _Otherwise ->
                    error_logger:info_report([{unknown_match_structure,
                                               JSON}]),
                    ok
            end;
        {<<"duel">>, <<"0">>} ->
            %% Unranked game, do not store it
            ok
    end.

add_new_player({Name, _, _} = In) ->
    case qlg_pgsql_srv:select_player(Name) of
        {ok, _, []} ->
            lager:debug("Adding new player ~p", [Name]),
            ok = qlg_pgsql_srv:mk_player(Name),
            add_new_player(In);
        {ok, _, [{Id, Name}]} ->
            {ok, Id}
    end.

decode_timestamp(Bin) when is_binary(Bin) ->
    {ok, [Month, Day, Year, HH, MM, AMPM], ""} =
        io_lib:fread("~u/~u/~u ~u:~u ~s", binary_to_list(Bin)),
    Date = {Year, Month, Day},
    Time = {case AMPM of
                "AM" -> HH;
                "PM" -> HH + 12
            end, MM, 0},
    {Date, Time}.

extract_scores(Obj) ->
    case {proplists:get_value(<<"PLAYER_NICK">>, Obj),
          proplists:get_value(<<"RANK">>, Obj),
          proplists:get_value(<<"SCORE">>, Obj)} of
        {undefined, _, _} ->
            exit(invariant_breach);
        {_, undefined, _} ->
            exit(invariant_breach);
        {_, _, undefined} ->
            exit(invariant_breach);
        {Player, Rank, Score} ->
            {Player, decode_rank(Rank), Score}
    end.

decode_rank(<<"1">>) -> 1;
decode_rank(<<"2">>) -> 2;
decode_rank(<<"-1">>) -> 999.

mk_match(Played, {Id1, {_P1, R1, S1}},
                 {Id2, {_P2, R2, S2}}) when R1 < R2 ->
    {ok, #duel_match { played = Played,
                       winner = Id1, winner_score = S1,
                       loser  = Id2, loser_score = S2 }};
mk_match(Played, {Id1, {_P1, R1, S1}},
                 {Id2, {_P2, R2, S2}}) when R2 < R1 ->
    {ok, #duel_match { played = Played,
                       winner = Id2, winner_score = S2,
                       loser = Id1, loser_score = S1 }};
mk_match(Played, {Id1, {_P1, 999, S1}},
                 {Id2, {_P2, 999, S2}}) when S1 > S2 ->
    {ok, #duel_match { played = Played,
                       winner = Id1, winner_score = S1,
                       loser  = Id2, loser_score = S2 }};
mk_match(Played, {Id1, {_P1, 999, S1}},
                 {Id2, {_P2, 999, S2}}) when S1 < S2 ->
    {ok, #duel_match { played = Played,
                       winner = Id2, winner_score = S2,
                       loser  = Id1, loser_score = S1 }};
mk_match(Played, {Id1, {_P1, 999, S}},
                 {Id2, {_P2, 999, S}}) ->
    {ok, #duel_match { played = Played,
                       winner = Id1, winner_score = S,
                       loser  = Id2, loser_score  = S}}.
