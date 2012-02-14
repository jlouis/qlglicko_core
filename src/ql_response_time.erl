-module(ql_response_time).
-behaviour(jobs_sampler).
-define(TAB, response_time_table).

-export([init/2,
         sample/2,
         handle_msg/3,
         calc/2,
         overload/1
        ]).

-record(state, {levels = [],
                count  = 0 }).

default_levels() ->
    [{3, 1}].

init(_Name, Opts) ->
    Levels = proplists:get_value(level, Opts, default_levels()),
    {ok, #state { levels = Levels, count = 0}}.

overload(Name) ->
    jobs_sampler:tell_sampler(Name, overload).

handle_msg(overload, _T, #state { count = K } = S) ->
    ok = lager:debug("Logging overload"),
    {ignore, S#state { count = K+1}};
handle_msg(Msg, _T, S) ->
    ok = lager:debug("Ignoring message ~p", [Msg]),
    {ignore, S}.

sample(_T, #state { count = K} = S) ->
    ok = lager:debug("Sampling, count ~B", [K]),
    {K, S#state { count = case K of 0 -> 0;
                               N  when N > 0 -> N-1 end}}.

calc(History, #state{levels = Levels} = S) ->
    L = jobs_sampler:calc(value, Levels, History),
    {[{response_time, L}], S}.
