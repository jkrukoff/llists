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

from_map_test() ->
    ?assertEqual([{1, one}, {2, two}, {3, three}],
                 llists:to_list(
                   llists:sort(
                     llists:from_map(#{1=>one, 2=>two, 3=>three})))).

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

to_map_test() ->
    ?assertEqual(#{1=>one, 2=>two, 3=>three},
                 llists:to_map(
                   llists:from_list([{1, one}, {2, two}, {3, three}]))).

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

keydelete_test() ->
    ?assertEqual([one, {three, 3}],
                 llists:to_list(
                   llists:keydelete(
                     two,
                     1,
                     llists:from_list([one, {two, 2}, {three, 3}])))).

keymap_test() ->
    ?assertEqual([{name, "jane", 22},{name, "lizzie", 20},{name, "lydia", 15}],
                 llists:to_list(
                   llists:keymap(
                     fun erlang:atom_to_list/1,
                     2,
                     llists:from_list(
                       [{name, jane, 22}, {name, lizzie, 20}, {name, lydia, 15}])))).

keymerge_test() ->
    ?assertEqual([{name, jane, 22},{name, lizzie, 20},{name, lydia, 15}],
                 llists:to_list(
                   llists:keymerge(
                     2,
                     llists:from_list([{name, jane, 22}]),
                     llists:from_list([{name, lizzie, 20}, {name, lydia, 15}])))).

keyreplace_test() ->
    ?assertEqual([one, {replaced}, {three, 3}],
                 llists:to_list(
                   llists:keyreplace(
                     two,
                     1,
                     llists:from_list([one, {two, 2}, {three, 3}]),
                     {replaced}))).

keysort_test() ->
    ?assertEqual([{one, 1}, {two, 2}, {three, 3}],
                 llists:to_list(
                   llists:keysort(
                     2,
                     llists:from_list([{three, 3}, {two, 2}, {one, 1}])))).

keystore_test_() ->
    [?_assertEqual([one, {stored}, {three, 3}],
                   llists:to_list(
                     llists:keystore(
                       two,
                       1,
                       llists:from_list([one, {two, 2}, {three, 3}]),
                       {stored}))),
     ?_assertEqual([one, {two, 2}, {three, 3}, {stored}],
                   llists:to_list(
                     llists:keystore(
                       four,
                       1,
                       llists:from_list([one, {two, 2}, {three, 3}]),
                       {stored})))].

keytake_test() ->
    {value, Value, Iterator} = llists:keytake(
                                 two,
                                 1,
                                 llists:from_list([one, {two, 2}, {three, 3}])),
    ?assertEqual({{two, 2}, [one, {three, 3}]},
                 {Value, llists:to_list(Iterator)}).

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

keyfind_test() ->
    ?assertEqual({two, 2},
                 llists:keyfind(
                   two,
                   1,
                   llists:from_list([{one, 1}, {two, 2}, {three, 3}]))).

keymember_test() ->
    ?assert(llists:keymember(
              two,
              1,
              llists:from_list([one, {two, 2}, {three, 3}]))).

keysearch_test() ->
    ?assertEqual({value, {two, 2}},
                 llists:keysearch(
                   two,
                   1,
                   llists:from_list([{one, 1}, {two, 2}, {three, 3}]))).

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

ukeymerge_test() ->
    ?assertEqual([{name, jane, 22},{name, lizzie, 20},{name, lydia, 15}],
                 llists:to_list(
                   llists:ukeymerge(
                     2,
                     llists:from_list([{name, jane, 22}, {name, lizzie, 20}]),
                     llists:from_list([{name, lizzie, 25}, {name, lydia, 15}])))).

ukeysort_test() ->
    ?assertEqual([{one, 1}, {two, 2}, {three, 3}],
                 llists:to_list(
                   llists:ukeysort(
                     2,
                     llists:from_list([{two, 2}, {three, 3}, {two, 2}, {one, 1}])))).


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

umerge3_test() ->
    ?assertEqual([1, 2, 3, 4, 5],
                 llists:to_list(
                   llists:umerge3(
                     llists:from_list([1, 2, 5]),
                     llists:from_list([2, 4]),
                     llists:from_list([3, 4])))).

unzip_test() ->
    {First, Second} = llists:unzip(
                        llists:from_list([{1, 4}, {2, 5}, {3, 6}])),
    ?assertEqual({[1, 2, 3], [4, 5, 6]},
                 {llists:to_list(First), llists:to_list(Second)}).

unzip3_test() ->
    {First, Second, Third} = llists:unzip3(
                               llists:from_list([{1, 3, 5}, {2, 4, 6}])),
    ?assertEqual({[1, 2], [3, 4], [5, 6]},
                 {llists:to_list(First),
                  llists:to_list(Second),
                  llists:to_list(Third)}).

usort_1_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:usort(
                     llists:from_list([3, 2, 2, 1])))).

usort_2_test() ->
    ?assertEqual([1, 2, 3],
                 llists:to_list(
                   llists:usort(
                     fun (A, B) -> A =< B end,
                     llists:from_list([3, 2, 2, 1])))).

zip_test_() ->
    [?_assertEqual([{1, 4}, {2, 5}, {3, 6}],
                   llists:to_list(
                     llists:zip(
                       llists:from_list([1, 2, 3]),
                       llists:from_list([4, 5, 6])))),
     ?_assertError(function_clause,
                   llists:to_list(
                     llists:zip(
                       llists:from_list([1, 2]),
                       llists:from_list([4, 5, 6]))))].

zip3_test_() ->
    [?_assertEqual([{1, 3, 5}, {2, 4, 6}],
                   llists:to_list(
                     llists:zip3(
                       llists:from_list([1, 2]),
                       llists:from_list([3, 4]),
                       llists:from_list([5, 6])))),
     ?_assertError(function_clause,
                   llists:to_list(
                     llists:zip3(
                       llists:from_list([1, 2]),
                       llists:from_list([3]),
                       llists:from_list([5, 6]))))].

zipwith_test_() ->
    [?_assertEqual([{1, 4}, {2, 5}, {3, 6}],
                   llists:to_list(
                     llists:zipwith(
                       fun (A, B) -> {A, B} end,
                       llists:from_list([1, 2, 3]),
                       llists:from_list([4, 5, 6])))),
     ?_assertError(function_clause,
                   llists:to_list(
                     llists:zipwith(
                       fun (A, B) -> {A, B} end,
                       llists:from_list([1, 2]),
                       llists:from_list([4, 5, 6]))))].

zipwith3_test_() ->
    [?_assertEqual([{1, 3, 5}, {2, 4, 6}],
                   llists:to_list(
                     llists:zipwith3(
                       fun (A, B, C) -> {A, B, C} end,
                       llists:from_list([1, 2]),
                       llists:from_list([3, 4]),
                       llists:from_list([5, 6])))),
     ?_assertError(function_clause,
                   llists:to_list(
                     llists:zipwith3(
                       fun (A, B, C) -> {A, B, C} end,
                       llists:from_list([1, 2]),
                       llists:from_list([3]),
                       llists:from_list([5, 6]))))].

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
