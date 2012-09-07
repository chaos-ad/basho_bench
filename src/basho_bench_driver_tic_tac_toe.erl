-module(basho_bench_driver_tic_tac_toe).

-export([new/1, run/4]).

-include("basho_bench.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

new(ID) ->
    ?INFO("Starting the TicTacToe driver with id ~p~n", [ID]),

    Cookie = basho_bench_config:get(tic_tac_toe_cookie),
    MyNode = basho_bench_config:get(tic_tac_toe_mynode),
    GLNode = basho_bench_config:get(tic_tac_toe_glnode),

    case net_kernel:start([MyNode, shortnames]) of
        {ok, _} ->
            ?INFO("Net kernel started as ~p~n", [node()]);
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            ?FAIL_MSG("Failed to start net_kernel for ~p: ~p~n", [?MODULE, Reason])
    end,

    ?INFO("Set cookie to ~p~n", [Cookie]),
    true = erlang:set_cookie(node(), Cookie),

    ?INFO("Try to ping ~p~n", [GLNode]),
    case net_kernel:connect(GLNode) of
        true  -> ok;
        false -> ?FAIL_MSG("Failed to connect connect ~p, aborting!~n", [?MODULE, GLNode])
    end,

    {ok, #state{node=GLNode}}.

run(create, KeyGen, _ValueGen, State) ->
    GameID = KeyGen(),
    ?INFO("Starting the TicTacToe game: ~p~n", [GameID]),
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
