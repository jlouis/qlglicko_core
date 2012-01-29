-module(qlg_fetch_match).

-include("match.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { id }).

%%%===================================================================

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

run(Pid) ->
    gen_server:cast(Pid, run).

%%%===================================================================

%% @private
init([Id]) ->
    true = gproc:add_local_name({fetch_match, Id}),
    {ok, #state{ id = Id }}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(run, State) ->
    fetch_and_store(State),
    {stop, normal, State};
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

fetch_and_store(#state { id = Id }) ->
    {ok, JSON} = ql_fetch:match(Id),
    persist_duel_match(Id, JSON),
    %% Do this last as a confirmation we got through the other parts
    %% This ensures an idempotent database.
    {ok, 1} = qlg_pgsql_srv:update_match(
                Id,
                term_to_binary(JSON, [compressed])),
    ok.

persist_duel_match(Id, JSON) ->
    case {proplists:get_value(<<"GAME_TYPE">>, JSON),
          proplists:get_value(<<"RANKED">>, JSON)} of
        {<<"duel">>, <<"1">>} ->
            %% Duel game type. And ranked. Find the winner and the loser
            [P1, P2] = proplists:get_value(<<"SCOREBOARD">>, JSON),
            Played = proplists:get_value(<<"GAME_TIMESTAMP">>, JSON),
            M = mk_match(decode_timestamp(Played),
                         extract_scores(P1),
                         extract_scores(P2)),
            add_new_player(P1),
            add_new_player(P2),
            qlg_pgsql_srv:store_match(Id, M),
            ok;
        {<<"duel">>, <<"0">>} ->
            ok
    end.

add_new_player({Name, _, _}) ->
    case qlg_pgsql_srv:select_player(Name) of
        {ok, []} ->
            qlg_pgsql_srv:mk_player(Name);
        {ok, [_|_]} ->
            ok
    end.

decode_timestamp(Bin) when is_binary(Bin) ->
    {ok, [Month, Day, Year, HH, MM, AMPM], ""} =
        io_lib:fread("~u/~u/~u ~u:~u ~s"),
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

mk_match(Played, {P1, R1, S1}, {P2, R2, S2}) when R1 < R2 ->
    #duel_match { played = Played,
                  winner = P1, winner_score = S1,
                  loser  = P2, loser_score = S2 };
mk_match(Played, {P1, R1, S1}, {P2, R2, S2}) when R2 < R1 ->
    #duel_match { played = Played,
                  winner = P2, winner_score = S2,
                  loser = P1, loser_score = S1 }.
