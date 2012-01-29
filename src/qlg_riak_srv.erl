%%%-------------------------------------------------------------------
%%% @author Jesper Louis Andersen <jlouis@tiefling.local>
%%% @copyright (C) 2012, Jesper Louis Andersen
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2012 by Jesper Louis Andersen <jlouis@tiefling.local>
%%%-------------------------------------------------------------------
-module(qlg_riak_srv).

-include("match.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([store_match/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { socket }).

%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store_match(Id, RawContent) when is_binary(Id) ->
    call({store_match, Id, RawContent}).

%%%===================================================================

%% @private
init([]) ->
    gproc:add_local_name(riak_endpoint),
    {RiakHost, RiakPort} =
        gproc:get_env(l, qlglicko, riak_host, [app_env, error]),

    {ok, Pid} = riakc_pb_socket:start_link(RiakHost, RiakPort),
    {ok, #state{ socket = Pid}}.

%% @private
handle_call({lookup_match_raw, Id}, _From,
            #state { socket = RS = State}) ->
    Reply = lookup_match_raw(RS, Id),
    {reply, Reply, State};
handle_call({store_match, Id, JSON}, _From,
           #state { socket = RS } = State) ->
    Reply = persist_match(RS, Id, JSON),
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
call(M) ->
    gen_server:call(?SERVER, M, infinity).

lookup_match_raw(Sock, Id) ->
    case riakc_pb_socket:get(Sock, <<"matches_raw">>, Id) of
        {ok, Obj} ->
            decode_term(Obj);
        {error, Reason} ->
            {error, Reason}
    end.

persist_match(Sock, Id, JSON) ->
    case riakc_pb_socket:get(Sock, <<"matches_raw">>, Id) of
        {error, not_found} ->
            ok = persist_raw(Sock, Id, term_to_binary(JSON)),
            ok = persist_duel_match(Sock, Id, JSON);
        {ok, _} ->
            %% Already stored, ignore
            ok
    end.

persist_raw(Sock, Id, Bin) when is_binary(Id),
                                is_binary(Bin) ->
    Object = create_term(<<"matches_raw">>, Id, Bin),
    riakc_pb_socket:put(Sock, Object).                            

persist_duel_match(Sock, Id, JSON) ->
    case {proplists:get_value(<<"GAME_TYPE">>, JSON),
          proplists:get_value(<<"RANKED">>, JSON)} of
        {<<"duel">>, <<"1">>} ->
            %% Duel game type. And ranked. Find the winner and the loser
            [P1, P2] = proplists:get_value(<<"SCOREBOARD">>, JSON),
            M = mk_match(extract_scores(P1), extract_scores(P2)),
            Object = create_term(<<"match_scores">>, Id, M),
            riakc_pb_socket:put(Sock, Object);
        _Otherwise ->
            %% Not a duel, do nothing
            ok
    end.

extract_scores(Obj) ->
    case {proplists:get_value(<<"PLAYER_NICK">>, Obj),
          proplists:get_value(<<"SCORE">>, Obj)} of
        {undefined, _} ->
            exit(invariant_breach);
        {_, undefined} ->
            exit(invariant_breach);
        {Player, Score} ->
            {Player, Score}
    end.

mk_match({P1, S1}, {P2,S2}) when S1 > S2 ->
    #duel_match { winner = P1, winner_score = S1,
             loser  = P2, loser_score = S2 };
mk_match({P1, S1}, {P2, S2}) when S2 > S1 ->
    #duel_match { winner = P2, winner_score = S2,
             loser = P1, loser_score = S1 }.

create_term(Bucket, Id, Term) ->
    riakc_obj:new(Bucket, Id, term_to_binary(Term, [compressed]),
                  <<"application/x-erlang-term">>).

encode_term(Object, Term) ->
  riakc_obj:update_value(Object, term_to_binary(Term, [compressed]),
                         <<"application/x-erlang-term">>).

decode_term(Object) ->
  case riakc_obj:get_content_type(Object) of
    <<"application/x-erlang-term">> ->
      try
        {ok, binary_to_term(riakc_obj:get_value(Object))}
      catch
        _:Reason ->
          {error, Reason}
      end;
    Ctype ->
      {error, {unknown_ctype, Ctype}}
  end.
