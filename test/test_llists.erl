%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/llists.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_llists).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

from_list_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:from_list([1, 2, 3]))).

unfold_test() ->
    ?assertEqual([3, 2, 1],
                 llists:to_list(
                   llists:unfold(fun (0) -> none;
                                     (Acc) -> {Acc, Acc - 1}
                                 end,
                                 3))).

duplicate_test_() ->
    [?_assertEqual([value, value, value],
                   llists:to_list(
                     llists:sublist(
                       llists:duplicate(infinity, value),
                       3))),
     ?_assertEqual([value, value, value],
                   llists:to_list(
                     llists:duplicate(3, value)))].

seq_test_() ->
    [?_assertEqual([1, 2, 3],
                   llists:to_list(
                     llists:sublist(
                       llists:seq(1, infinity),
                       3))),
     ?_assertEqual([-1, -2, -3],
                   llists:to_list(
                     llists:sublist(
                       llists:seq(-1, '-infinity', -1),
                       3))),
     ?_assertEqual([1, 2, 3],
                   llists:to_list(
                     llists:seq(1, 3))),
     ?_assertEqual([10, 5],
                   llists:to_list(
                     llists:seq(10, 1, -5))),
     ?_assertEqual([1],
                   llists:to_list(
                     llists:seq(1, 1, 0)))].

is_iterator_test_() ->
    [?_assert(llists:is_iterator(
                llists:from_list([1, 2, 3]))),
     ?_assertNot(llists:is_iterator(value))].

next_test() ->
    [Head | Tail] = llists:next(llists:from_list([1, 2, 3])),
    ?assertEqual(1, Head),
    ?assert(llists:is_iterator(Tail)).

hd_test() ->
    ?assertEqual(1,
                 llists:hd(
                   llists:from_list([1, 2, 3]))).

length_test() ->
    ?assertEqual(3,
                 llists:length(
                   llists:from_list([1, 2, 3]))).

tl_test() ->
    ?assert(llists:is_iterator(
              llists:tl(
                llists:from_list([1, 2, 3])))).

to_list_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:from_list([1, 2, 3]))).

all_test() ->
    ?assertEqual(true,
                 llists:all(fun (Elem) -> Elem > 0 end,
                            llists:from_list([1, 2, 3]))).

any_test() ->
    ?assertEqual(true,
                 llists:any(fun (Elem) -> Elem == 3 end,
                            llists:from_list([1, 2, 3]))).

append_1_test() ->
    ?assertEqual([1, 2, 3, 4],
                 llists:to_list(
                   llists:append(
                     llists:from_list([llists:from_list([1, 2]),
                                       llists:from_list([3, 4])])))).

append_2_test() ->
    ?assertEqual([1, 2, 3, 4],
                 llists:to_list(
                   llists:append(
                     llists:from_list([1, 2]),
                     llists:from_list([3, 4])))).

concat_test() ->
    ?assertEqual("1two3.00000000000000000000e+00four",
                 llists:concat(
                   llists:from_list([1, two, 3.0, "four"]))).

delete_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:delete(
                     2,
                     llists:from_list([1, 2, 2, 3])))).

droplast_test_() ->
    [?_assertEqual([1, 2],
                   llists:to_list(
                     llists:droplast(
                       llists:from_list([1, 2, 3])))),
     ?_assertError(function_clause,
                   llists:to_list(
                     llists:droplast(
                       llists:from_list([]))))].

dropwhile_test() ->
    ?assertEqual([3, 4, 5],
                 llists:to_list(
                   llists:dropwhile(
                     fun (Elem) -> Elem < 3 end,
                     llists:from_list([1, 2, 3, 4, 5])))).

filter_test() ->
    ?assertEqual([2],
                 llists:to_list(
                   llists:filter(fun (Elem) -> Elem == 2 end,
                              llists:from_list([1, 2, 3])))).

join_test_() ->
    [?_assertEqual([a, x, b, x, c],
                   llists:to_list(
                     llists:join(
                       x,
                       llists:from_list([a, b, c])))),
     ?_assertEqual([a],
                   llists:to_list(
                     llists:join(
                       x,
                       llists:from_list([a])))),
     ?_assertEqual([],
                   llists:to_list(
                     llists:join(
                       x,
                       llists:from_list([]))))].

map_test() ->
    ?assertEqual([2, 4, 6],
                 llists:to_list(
                   llists:map(fun (E) -> E * 2 end,
                              llists:from_list([1, 2, 3])))).

merge_1_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:merge(
                     llists:from_list(
                       [llists:from_list([1, 5]),
                        llists:from_list([2, 3, 4])])))).

merge_2_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:merge(
                     llists:from_list([1, 5]),
                     llists:from_list([2, 3, 4])))).

merge_3_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:merge(
                     fun (A, B) -> A =< B end,
                     llists:from_list([1, 5]),
                     llists:from_list([2, 3, 4])))).

merge3_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:merge3(
                     llists:from_list([1, 5]),
                     llists:from_list([2, 4]),
                     llists:from_list([3])))).

nthtail_test_() ->
    [?_assertEqual([3, 4, 5],
                   llists:to_list(
                     llists:nthtail(
                       2,
                       llists:from_list([1, 2, 3, 4, 5])))),
     ?_assertError(function_clause,
                   llists:nthtail(
                     1,
                     llists:from_list([])))].

reverse_1_test() ->
    ?assertEqual([4, 3, 2, 1],
                 llists:to_list(
                   llists:reverse(
                     llists:from_list([1, 2, 3, 4])))).

reverse_2_test() ->
    ?assertEqual([4, 3, 2, 1, a, b, c],
                 llists:to_list(
                   llists:reverse(
                     llists:from_list([1, 2, 3, 4]),
                     llists:from_list([a, b, c])))).

sublist_2_test() ->
    ?assertEqual([1, 2],
                 llists:to_list(
                   llists:sublist(
                     llists:from_list([1, 2, 3]),
                     2))).

sublist_3_test_() ->
    [?_assertEqual([2, 3, 4],
                   llists:to_list(
                     llists:sublist(
                       llists:from_list([1, 2, 3, 4, 5]),
                       2,
                       3))),
     ?_assertEqual([2, 3, 4],
                   llists:to_list(
                     llists:sublist(
                       llists:from_list([1, 2, 3, 4]),
                       2,
                       10)))].

foldl_test() ->
    ?assertEqual([3, 2, 1],
                 llists:foldl(
                   fun (Elem, Acc) -> [Elem | Acc] end,
                   [],
                   llists:from_list([1, 2, 3]))).

foldr_test() ->
    ?assertEqual([1, 2, 3, void],
                 llists:foldr(
                   fun (A, AccIn) -> [A | AccIn] end,
                   [void],
                   llists:from_list([1, 2, 3]))).

foreach_test() ->
    ?assertEqual(ok,
                 llists:foreach(
                   fun (A) -> A end,
                   llists:from_list([1, 2, 3]))).

takewhile_test() ->
    ?assertEqual([1, 2],
                 llists:to_list(
                   llists:takewhile(
                     fun (Elem) -> Elem < 3 end,
                     llists:from_list([1, 2, 3, 4, 5])))).

partition_test() ->
    {S, NS} = llists:partition(
                fun (A) -> is_atom(A) end,
                llists:from_list([a, b, 1, c, d, 2, 3, 4, e])),
    ?assertEqual({[a, b, c, d, e], [1, 2, 3, 4]},
                 {llists:to_list(S), llists:to_list(NS)}).

split_test() ->
    {Before, After} = llists:split(
                        3,
                        llists:from_list([1, 2, 3, 4, 5])),
    ?assertEqual({[1, 2, 3], [4, 5]},
                 {llists:to_list(Before), llists:to_list(After)}).

splitwith_test() ->
    {Before, After} = llists:splitwith(
                        fun (Elem) -> Elem < 4 end,
                        llists:from_list([1, 2, 3, 4, 5])),
    ?assertEqual({[1, 2, 3], [4, 5]},
                 {llists:to_list(Before), llists:to_list(After)}).

subtract_test() ->
    ?assertEqual([3, 1, 2],
                 llists:to_list(
                   llists:subtract(
                     llists:from_list([1, 2, 3, 2, 1, 2]),
                     llists:from_list([2, 1, 2])))).

umerge_1_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:umerge(
                     llists:from_list(
                       [llists:from_list([1, 2, 5]),
                        llists:from_list([2, 3, 4])])))).

umerge_2_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:umerge(
                     llists:from_list([1, 2, 5]),
                     llists:from_list([2, 3, 4])))).

umerge_3_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:umerge(
                     fun (A, B) -> A =< B end,
                     llists:from_list([1, 2, 5]),
                     llists:from_list([2, 3, 4])))).

filtermap_test() ->
    ?assertEqual([1, 2],
                 llists:to_list(
                   llists:filtermap(
                     fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end,
                     llists:from_list([1, 2, 3, 4, 5])))).

flatlength_test() ->
    ?assertEqual(5,
                 llists:flatlength(
                   llists:from_list([llists:from_list([1, 2, 3]),
                                     llists:from_list([4, 5])]))).

flatmap_test() ->
    ?assertEqual([a, a, b, b, c, c],
                 llists:to_list(
                   llists:flatmap(
                     fun (X) -> llists:from_list([X, X]) end,
                     llists:from_list([a, b, c])))).

flatten_1_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:flatten(
                     llists:from_list(
                       [llists:from_list([llists:from_list([1])]),
                        2,
                        llists:from_list([3, 4]),
                        llists:from_list([]),
                        llists:from_list([5])])))).

flatten_2_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:flatten(
                     llists:from_list([llists:from_list([1, 2, 3])]),
                     llists:from_list([4, 5])))).

last_test_() ->
    [?_assertEqual(3,
                   llists:last(
                     llists:from_list([1, 2, 3]))),
     ?_assertError(function_clause,
                   llists:last(
                     llists:from_list([])))].

mapfoldl_test() ->
    {Mapped, Acc} = llists:mapfoldl(
                      fun(X, Sum) -> {2 * X, X + Sum} end,
                      0,
                      llists:from_list([1, 2, 3, 4, 5])),
    ?assertEqual({[2, 4, 6, 8, 10], 15},
                 {llists:to_list(Mapped), Acc}).

mapfoldr_test() ->
    {Mapped, Acc} = llists:mapfoldr(
                      fun(X, Sum) -> {2 * X, X + Sum} end,
                      0,
                      llists:from_list([1, 2, 3, 4, 5])),
    ?assertEqual({[2, 4, 6, 8, 10], 15},
                 {llists:to_list(Mapped), Acc}).

max_test() ->
    ?assertEqual(3,
                 llists:max(
                   llists:from_list([1, 2, 3]))).

member_test() ->
    ?assert(llists:member(
              1,
              llists:from_list([1, 2, 3]))).

min_test() ->
    ?assertEqual(1,
                 llists:min(
                   llists:from_list([1, 2, 3]))).

nth_test_() ->
    [?_assertEqual(3,
                   llists:nth(
                     3,
                     llists:from_list([1, 2, 3]))),
     ?_assertError(function_clause,
                   llists:nth(
                     4,
                     llists:from_list([1, 2, 3])))].

prefix_test() ->
    ?assert(llists:prefix(
              llists:from_list([1, 2, 3]),
              llists:from_list([1, 2, 3, 4, 5]))).

sort_1_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:sort(
                     llists:from_list([3, 2, 1])))).

sort_2_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:sort(
                     fun (A, B) -> A =< B end,
                     llists:from_list([3, 2, 1])))).

search_test() ->
    ?assertEqual({value, 2},
                 llists:search(
                   fun (Elem) -> Elem == 2 end,
                   llists:from_list([1, 2, 3]))).

suffix_test() ->
    ?assert(llists:suffix(
              llists:from_list([d, e, f]),
              llists:from_list([a, b, c, d, e, f]))).

sum_test() ->
    ?assertEqual(6,
                 llists:sum(
                   llists:from_list([1, 2, 3]))).
