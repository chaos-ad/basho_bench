-module(basho_bench_driver_tic_tac_toe).

-export([new/1, run/4]).

-include("basho_bench.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {node, game_id, user, turns}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

new(ID) ->
    ?INFO("Starting the TicTacToe driver with id ~p~n", [ID]),

    Cookie = basho_bench_config:get(tic_tac_toe_cookie, undefined),
    MyNode = basho_bench_config:get(tic_tac_toe_mynode),
    GLNode = basho_bench_config:get(tic_tac_toe_glnode),

    case net_kernel:start([MyNode]) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p~n", [node()]);
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p~n", [?MODULE, Reason])
    end,

    case Cookie =:= undefined of
        true  -> ok;
        false ->
            ?INFO("Set cookie to ~p~n", [Cookie]),
            true = erlang:set_cookie(node(), Cookie)
    end,

    ?INFO("Try to ping ~p~n", [GLNode]),
    case net_kernel:connect(GLNode) of
        true  -> ok;
        false -> ?FAIL_MSG("Failed to connect connect ~p, aborting!~n", [GLNode])
    end,

    GameID = gameid(),
    ok = rpc:call(GLNode, gl_control, start_game_instance, [GameID]),
    ok = rpc:call(GLNode, gl_control, user_join, [GameID, <<"User1">>]),
    ok = rpc:call(GLNode, gl_control, user_join, [GameID, <<"User2">>]),
    {ok, #state{node=GLNode, game_id=GameID, user = <<"User1">>, turns=new_turns()}}.

run(turn, _, _, State=#state{node=GLNode, game_id=GameID, user=User, turns=[Turn]}) ->
    ok = rpc:call(GLNode, gl_control, send_actions, [GameID, User, [{<<"front_msg">>, [cmd(Turn)]}]]),
    {ok, State#state{user=next_user(User), turns=new_turns()}};
run(turn, _, _, State=#state{node=GLNode, game_id=GameID, user=User, turns=[Turn|NextTurns]}) ->
    ok = rpc:call(GLNode, gl_control, send_actions, [GameID, User, [{<<"front_msg">>, [cmd(Turn)]}]]),
    {ok, State#state{user=next_user(User), turns=NextTurns}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd(N) ->
    iolist_to_binary(io_lib:format("click ~b", [N])).

next_user(<<"User1">>) -> <<"User2">>;
next_user(<<"User2">>) -> <<"User1">>.

gameid() ->
    <<ID:128/big-unsigned-integer>> = crypto:rand_bytes(16),
    list_to_binary( lists:flatten(io_lib:format("~32.16.0b", [ID])) ).

new_turns() ->
    [1,2,3,5,4,7,6,9,8].
