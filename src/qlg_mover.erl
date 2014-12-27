-module(qlg_mover).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% API
%% -------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% CALLBACKS
%% ------------------------

init([]) ->
    {ok, undefined}.
    
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(work, State) ->
    {ok, Limit} = application:get_env(qlglicko_core, matches_to_move),
    TS = os:timestamp(),
    ok = process_moves(Limit),
    EndTS = os:timestamp(),
    Diff = timer:now_diff(EndTS, TS) / (1000 * 1000),
    lager:info("Processed ~B moves in ~B seconds", [Limit, Diff]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Aux) ->
    {ok, State}.

%% INTERNALS
%% ---------------------------

process_moves(Count) ->
    {ok, Matches} = qlg_db:matches_to_move(Count),
    move_matches(Matches).

move_matches([]) -> ok;
move_matches([M|Ms]) ->
    ok = move_match(M),
    move_matches(Ms).
    
move_match({ID, Added, Content, Analyzed}) ->
    ok = qlg_db:store_match_json(ID, Added, move_content(Content), Analyzed),
    ok = qlg_db:mark_moved(ID),
    ok.
    
move_content(undefined) -> undefined;
move_content(Binary) ->
    Term = binary_to_term(Binary),
    iolist_to_binary(jsx:encode(Term)).
