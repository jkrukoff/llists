%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/llists_utils.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_llists_utils).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

cycle_test() ->
    ?assertEqual([1, 2, 3, 1, 2],
                 llists:to_list(
                   llists:sublist(
                     llists_utils:cycle(
                       llists:from_list([1, 2, 3])),
                     5))).

unique_1_test() ->
    ?assertEqual([1, 2, 3, 2, 1],
                 llists:to_list(
                   llists_utils:unique(
                     llists:from_list([1, 1.0, 2, 2.0, 2, 3, 2, 1])))).

unique_2_test() ->
    ?assertEqual([1, 2, 3, 2, 1],
                 llists:to_list(
                   llists_utils:unique(
                     fun (A, B) -> A =:= B end,
                     llists:from_list([1, 1, 2, 2, 2, 3, 2, 1])))).
