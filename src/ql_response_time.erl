-module(ql_response_time).
-behaviour(jobs_sampler).
-define(TAB, response_time_table).

-export([init/2,
         sample/2,
         handle_msg/3,
         calc/2]).
-export([report/1]).

%% Report a response time event
report(Ms) ->
    ets:update_counter(?TAB, count, 1),
    ets:update_counter(?TAB, sum, Ms).

-record(state, {levels = []}).

default_levels() -> [{4000,1},{8000,2},{12000,3}].

init(_Name, Opts) ->
    ets:new(?TAB, [named_table, public,
                   {write_concurrency, true}]),
    ets:insert(?TAB, {count, 0}),
    ets:insert(?TAB, {sum, 0}),
    Levels = proplists:get_value(level, Opts, default_levels()),
    {ok, #state { levels = Levels}}.

handle_msg(_Msg, _Timestamp, ModS) ->
    {ignore, ModS}.

sample(_Timestamp, #state{} = S) ->
    Result = fetch_from_ets(),
    {Result, S}.

fetch_from_ets() ->
    %% For the time being we use a simple average sampler
    Count = ets:lookup_element(?TAB, count, 2),
    Sum   = ets:lookup_element(?TAB, sum, 2),
    %% This may fluke in a race, but I don't care
    %% Assumption: If the system is fast you can live with less
    %% precise stats
    ets:insert(?TAB, {count, 0}),
    ets:insert(?TAB, {sum, 0}),
    case Count of
        0 -> undefined;
        _ -> Sum / Count
    end.

calc(History, #state{levels = Levels} = St) ->
    L = jobs_sampler:calc(value, Levels, History),
    {[{response_time, L}], St}.
