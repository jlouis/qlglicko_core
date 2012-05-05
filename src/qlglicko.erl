-module(qlglicko).

-export([rank/1, rank/2, rank_aux/1]).

rank(Id) ->
    rank(Id, []).

rank(Id, Opts) ->
    qlg_ranker:rank(Id, Opts).

players(Matches) ->
    lists:usort(
      lists:concat([[P1, P2] || {P1, P2, _} <- Matches])).

fetch_ratings() ->
    NewRatings = ets:match_object(qlrank_rate_result, {'_', '_', '_', '_'}),
    ets:match_delete(qlrank_rate_result, '_'),
    NewRatings.

glicko([]) -> done;
glicko([MatchObj | Next]) ->
    ok = rank(MatchObj()),
    glicko(Next).

rank_aux(Matches) ->
    Players = players(Matches),
    error_logger:info_report([players, Players]),
    glicko_db:add_new_players(Players),
    error_logger:info_report([db_content,
                              ets:match_object(ql_player_db, '$1')]),
    G = glicko_bg:initialize(Players, Matches),

    Avg = glicko_bg:average_matches(G, glicko_db:players()),
    error_logger:info_report([{average, Avg}]),

    PlayerList = glicko_db:players(),
    Jobs = length(PlayerList),
    set_jobs(Jobs),

    {ok, Ref} = rate(G, glicko_db:players()),

    receive
        {done, Ref} -> ok
    end,

    NewRatings = fetch_ratings(),
    glicko_db:update_rating(NewRatings),
    glicko_bg:delete(G),
    ok.

set_jobs(Jobs) ->
    ets:insert(qlrank_rate_result, {jobs, Jobs}).

rate(G, Ps) ->
    Master = self(),
    Ref = make_ref(),
    [spawn_link(
       fun () ->
               jobs:run(
                 qlrank,
                 fun () ->
                         rate_1(G, P),
                         case ets:update_counter(
                                qlrank_rate_result,
                                jobs,
                                {2, -1}) of
                             0 ->
                                 Master ! {done, Ref};
                             K when K rem 10 == 0 ->
                                 io:format("Jobs left: ~B~n", [K]),
                                 ok;
                             _ ->
                                 ok
                         end
                 end)
       end) || P <- Ps],
    {ok, Ref}.

rate_1(G, [Player]) ->
    [{Player, R, RD, Sigma}] = glicko_db:lookup(Player),
    Rating = case glicko_bg:matches(G, Player) of
                 [] ->
                     %% No opponents for this tournament
                     RD1 = glicko2:phi_star(RD, Sigma),
                     {Player, R, RD1, Sigma};
                 Opponents ->
                     {R1, RD1, Sigma1} =
                         glicko2:rate(R, RD, Sigma, Opponents),
                     {Player, R1, RD1, Sigma1}
             end,
    ets:insert(qlrank_rate_result, Rating),
    ok.


