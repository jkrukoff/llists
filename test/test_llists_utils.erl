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

choice_test() ->
    ?assertEqual([1, 1, 1],
                 llists:to_list(
                   llists:sublist(
                     llists_utils:choice([1]),
                     3))).

combinations_2_test() ->
    ?assertEqual([[1,2,3], [1,2,4], [1,2,5],
                  [1,3,4], [1,3,5], [1,4,5],
                  [2,3,4], [2,3,5], [2,4,5],
                  [3,4,5]],
                 llists:to_list(
                   llists_utils:combinations(3, [1, 2, 3, 4, 5]))).

combinations_3_test_() ->
    [?_assertEqual([[1,2,3], [1,2,4], [1,2,5],
                    [1,3,4], [1,3,5], [1,4,5],
                    [2,3,4], [2,3,5], [2,4,5],
                    [3,4,5]],
                   llists:to_list(
                     llists_utils:combinations(3, [1, 2, 3, 4, 5], []))),
     ?_assertEqual([[1,1,1], [1,1,2], [1,1,3],
                    [1,1,4], [1,2,2], [1,2,3],
                    [1,2,4], [1,3,3], [1,3,4],
                    [1,4,4], [2,2,2], [2,2,3],
                    [2,2,4], [2,3,3], [2,3,4],
                    [2,4,4], [3,3,3], [3,3,4],
                    [3,4,4], [4,4,4]],
                   llists:to_list(
                     llists_utils:combinations(3, [1, 2, 3, 4], [repetitions])))].

cycle_test() ->
    ?assertEqual([1, 2, 3, 1, 2],
                 llists:to_list(
                   llists:sublist(
                     llists_utils:cycle(
                       llists:from_list([1, 2, 3])),
                     5))).

enumerate_test() ->
    ?assertEqual([{1, one}, {2, two}],
                 llists:to_list(
                   llists_utils:enumerate(
                     llists:from_list([one, two])))).

group_test_() ->
    [?_assertEqual([],
                   llists:to_list(
                     llists_utils:group(
                       2,
                       llists:from_list([])))),
     ?_assertEqual([[1, 2], [3]],
                   llists:to_list(
                     llists_utils:group(
                       2,
                       llists:from_list([1, 2, 3])))),
     ?_assertEqual([[1, 2], [3, 4]],
                   llists:to_list(
                     llists_utils:group(
                       2,
                       llists:from_list([1, 2, 3, 4]))))].

groupwith_test_() ->
    IsEven = fun (Elem) -> Elem rem 2 == 0 end,
    [?_assertEqual([],
                   llists:to_list(
                     llists_utils:groupwith(
                       IsEven,
                       llists:from_list([])))),
     ?_assertEqual([[1, 2], [3]],
                   llists:to_list(
                     llists_utils:groupwith(
                       IsEven,
                       llists:from_list([1, 2, 3])))),
     ?_assertEqual([[1, 2], [3, 4]],
                   llists:to_list(
                     llists_utils:groupwith(
                       IsEven,
                       llists:from_list([1, 2, 3, 4]))))].

permutations_2_test() ->
    ?assertEqual([[1,2,3], [1,2,4], [1,3,2],
                  [1,3,4], [1,4,2], [1,4,3],
                  [2,1,3], [2,1,4], [2,3,1],
                  [2,3,4], [2,4,1], [2,4,3],
                  [3,1,2], [3,1,4], [3,2,1],
                  [3,2,4], [3,4,1], [3,4,2],
                  [4,1,2], [4,1,3], [4,2,1],
                  [4,2,3], [4,3,1], [4,3,2]],
                 llists:to_list(
                   llists_utils:permutations(3, [1, 2, 3, 4]))).

permutations_3_test_() ->
    [?_assertEqual([[1,2,3], [1,2,4], [1,3,2],
                    [1,3,4], [1,4,2], [1,4,3],
                    [2,1,3], [2,1,4], [2,3,1],
                    [2,3,4], [2,4,1], [2,4,3],
                    [3,1,2], [3,1,4], [3,2,1],
                    [3,2,4], [3,4,1], [3,4,2],
                    [4,1,2], [4,1,3], [4,2,1],
                    [4,2,3], [4,3,1], [4,3,2]],
                   llists:to_list(
                     llists_utils:permutations(3, [1, 2, 3, 4], []))),
     ?_assertEqual([[1,1,1], [1,1,2], [1,1,3],
                    [1,1,4], [1,2,1], [1,2,2],
                    [1,2,3], [1,2,4], [1,3,1],
                    [1,3,2], [1,3,3], [1,3,4],
                    [1,4,1], [1,4,2], [1,4,3],
                    [1,4,4], [2,1,1], [2,1,2],
                    [2,1,3], [2,1,4], [2,2,1],
                    [2,2,2], [2,2,3], [2,2,4],
                    [2,3,1], [2,3,2], [2,3,3],
                    [2,3,4], [2,4,1], [2,4,2],
                    [2,4,3], [2,4,4], [3,1,1],
                    [3,1,2], [3,1,3], [3,1,4],
                    [3,2,1], [3,2,2], [3,2,3],
                    [3,2,4], [3,3,1], [3,3,2],
                    [3,3,3], [3,3,4], [3,4,1],
                    [3,4,2], [3,4,3], [3,4,4],
                    [4,1,1], [4,1,2], [4,1,3],
                    [4,1,4], [4,2,1], [4,2,2],
                    [4,2,3], [4,2,4], [4,3,1],
                    [4,3,2], [4,3,3], [4,3,4],
                    [4,4,1], [4,4,2], [4,4,3],
                    [4,4,4]],
                   llists:to_list(
                     llists_utils:permutations(3, [1, 2, 3, 4], [repetitions])))].

random_0_test() ->
    [Value | _] = llists:next(llists_utils:random()),
    ?assert(is_float(Value)).

random_1_test() ->
    [Value | _] = llists:next(llists_utils:random(10)),
    ?assert(is_integer(Value)).

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
