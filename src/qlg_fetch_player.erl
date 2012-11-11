-module(qlg_fetch_player).

-behaviour(gen_server).

%% API
-export([start_link/3, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,
	{ id,
           name,
           age}).

%%%===================================================================

start_link(Id, Name, Age) ->
    gen_server:start_link(?MODULE, [Id, Name, Age], []).

run(Pid) ->
    gen_server:cast(Pid, run).

%%%===================================================================

%% @private
init([Id, Name, Age]) ->
    true = gproc:add_local_name({fetch_player, Name}),
    {ok, #state{ id = Id, name = Name, age = Age }}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(run, State) ->
    %% @todo handle errors and overloads
    case jobs:ask(ql_fetch) of
        {ok, _Opaque} ->
            case qlg_overload:ask() of
                yes ->
                    run_fetch_job(State);
                no ->
                    ok
            end,
            {stop, normal, State}
    end;
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

universal_date() ->
    {Date, _} = calendar:universal_time(),
    Date.


weeks_behind(Now, W) ->
    Days = calendar:date_to_gregorian_days(Now),
    calendar:gregorian_days_to_date(Days - W*7).

find_weeks(W) ->
    find_weeks(universal_date(), W).

find_weeks(Now, K) when K < 7 ->
    [Now];
find_weeks(Now, K) when K >= 7 ->
    WeeksBehind = K div 7,
    [weeks_behind(Now, WeeksBehind) | find_weeks(Now, K-7) ].

fetch_and_store(#state { id = Id, name = Name, age = Age}) ->
    ok = lager:debug("Refreshing player ~s, ~p days behind, for weeks ~p", [Name, Age, WeeksToFetch]),
    WeeksToFetch = find_weeks(Age),
    case ql_fetch:player_matches(Name, WeeksToFetch) of
      account_closed ->
        {ok, _} = qlg_pgsql_srv:add_to_hall_of_fame(Id, Name),
        {ok, _} = qlg_pgsql_srv:remove_active_player(Id),
        lager:debug("Player account ~s closed, player moved to Hall of fame"),
        ok;
      {ok, Matches} ->
        R = [{ok, _} = qlg_pgsql_srv:store_match(M, null) || M <- Matches],
        Count = lists:sum([K || {ok, K} <- R]),
        lager:debug("Player ~s adding ~B new matches to fetch queue", [Name, Count]),
        {ok, 1} = qlg_pgsql_srv:refresh_player(Id),
        ok
    end.

run_fetch_job(#state { id = Id } = State) ->
    case qlg_pgsql_srv:should_player_be_refreshed(Id) of
      true -> fetch_and_store(State);
      false -> ok
    end.
	