%%%-------------------------------------------------------------------
%%% @doc
%%% Property tests for src/llists.erl
%%% @end
%%%-------------------------------------------------------------------
-module(prop_llists).

-include_lib("proper/include/proper.hrl").

%% Property options.
-export([]).

%%%===================================================================
%%% Tests
%%%===================================================================

prop_from_list() ->
    ?FORALL(
        List,
        list(),
        list_wrap(fun(L) -> L end, List) == List
    ).

prop_from_map() ->
    ?FORALL(
        Map,
        map(any(), any()),
        llists:to_map(llists:from_map(Map)) == Map
    ).

prop_duplicate() ->
    ?FORALL(
        {Count, Term},
        {integer(0, inf), any()},
        llists:to_list(llists:duplicate(Count, Term)) ==
            lists:duplicate(Count, Term)
    ).

prop_seq_2() ->
    ?FORALL(
        {From, To},
        range(inf),
        llists:to_list(llists:seq(From, To)) ==
            lists:seq(From, To)
    ).

prop_seq_3() ->
    ?FORALL(
        {From, To, Incr},
        ?LET({From, To}, range(inf), {From, To, sign(To - From)}),
        llists:to_list(llists:seq(From, To, Incr)) ==
            lists:seq(From, To, Incr)
    ).

prop_next() ->
    ?FORALL(
        List,
        list(),
        begin
            case llists:next(llists:from_list(List)) of
                [] ->
                    List == [];
                [Elem | Iterator] ->
                    (Elem == hd(List)) and llists:is_iterator(Iterator)
            end
        end
    ).

prop_hd() ->
    ?FORALL(
        List,
        non_empty(list()),
        llists:hd(llists:from_list(List)) == hd(List)
    ).

prop_tl() ->
    ?FORALL(
        List,
        non_empty(list()),
        llists:is_iterator(llists:tl(llists:from_list(List)))
    ).

prop_append_1() ->
    ?FORALL(
        List,
        list(small_list()),
        list_wrap(fun llists:append/1, List) == lists:append(List)
    ).

prop_append_2() ->
    ?FORALL(
        {List1, List2},
        {list(), list()},
        llists:to_list(
            llists:append(
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:append(List1, List2)
    ).

prop_delete() ->
    ?FORALL(
        {Elem, List},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains(Elem)})},
            {1, {any(), list()}}
        ]),
        list_wrap(fun(L) -> llists:delete(Elem, L) end, List) ==
            lists:delete(Elem, List)
    ).

prop_droplast() ->
    ?FORALL(
        List,
        non_empty(list()),
        list_wrap(fun llists:droplast/1, List) == lists:droplast(List)
    ).

prop_dropwhile() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        begin
            list_wrap(fun(L) -> llists:dropwhile(Pred, L) end, List) ==
                lists:dropwhile(Pred, List)
        end
    ).

prop_enumerate_1() ->
    ?FORALL(
        List,
        list(),
        llists:to_list(
            llists:enumerate(
                llists:from_list(List)
            )
        ) == lists:enumerate(List)
    ).

prop_enumerate_2() ->
    ?FORALL(
        {I, List},
        {integer(), list()},
        llists:to_list(
            llists:enumerate(
                I, llists:from_list(List)
            )
        ) == lists:enumerate(I, List)
    ).

prop_filter() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        begin
            list_wrap(fun(L) -> llists:filter(Pred, L) end, List) ==
                lists:filter(Pred, List)
        end
    ).

prop_filtermap() ->
    ?FORALL(
        {FilterMap, List},
        {function(1, union([boolean(), {true, any()}])), list(integer())},
        begin
            list_wrap(
                fun(L) -> llists:filtermap(FilterMap, L) end,
                List
            ) ==
                lists:filtermap(FilterMap, List)
        end
    ).

prop_flatlength() ->
    ?FORALL(
        List,
        deep_list(),
        llists:flatlength(from_deep_list(List)) ==
            lists:flatlength(List)
    ).

prop_flatmap() ->
    ?FORALL(
        {Map, List},
        {function(1, small_list()), list()},
        begin
            IteratorMap = fun(E) -> llists:from_list(Map(E)) end,
            list_wrap(
                fun(L) -> llists:flatmap(IteratorMap, L) end,
                List
            ) ==
                lists:flatmap(Map, List)
        end
    ).

prop_flatten_1() ->
    ?FORALL(
        List,
        deep_list(),
        llists:to_list(llists:flatten(from_deep_list(List))) ==
            lists:flatten(List)
    ).

prop_flatten_2() ->
    ?FORALL(
        {List, Tail},
        {deep_list(), list()},
        llists:to_list(
            llists:flatten(
                from_deep_list(List),
                llists:from_list(Tail)
            )
        ) ==
            lists:flatten(List, Tail)
    ).

prop_join() ->
    ?FORALL(
        {Sep, List},
        {any(), list()},
        list_wrap(fun(L) -> llists:join(Sep, L) end, List) ==
            lists:join(Sep, List)
    ).

prop_keydelete() ->
    ?FORALL(
        {Key, {N, List}},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
            {1, {any(), {pos_integer(), list(tuplish())}}}
        ]),
        list_wrap(fun(L) -> llists:keydelete(Key, N, L) end, List) ==
            lists:keydelete(Key, N, List)
    ).

prop_keymap() ->
    ?FORALL(
        {Map, {N, List}},
        {function(1, any()), key_list()},
        begin
            list_wrap(fun(L) -> llists:keymap(Map, N, L) end, List) ==
                lists:keymap(Map, N, List)
        end
    ).

prop_keymerge() ->
    ?FORALL(
        {N, List1, List2},
        ?LET(
            {N, List},
            key_list(),
            {N, keysortedish(N, List), keysortedish(N, key_list(any(), N))}
        ),
        llists:to_list(
            llists:keymerge(
                N,
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:keymerge(N, List1, List2)
    ).

prop_keyreplace() ->
    ?FORALL(
        {Key, {N, List}, Replace},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem), tuple()})},
            {1, {any(), {pos_integer(), list(tuplish())}, tuple()}}
        ]),
        list_wrap(fun(L) -> llists:keyreplace(Key, N, L, Replace) end, List) ==
            lists:keyreplace(Key, N, List, Replace)
    ).

prop_keysort() ->
    ?FORALL(
        {N, List},
        key_list(),
        list_wrap(fun(L) -> llists:keysort(N, L) end, List) ==
            lists:keysort(N, List)
    ).

prop_keystore() ->
    ?FORALL(
        {Key, {N, List}, Replace},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem), tuple()})},
            {1, {any(), {pos_integer(), list(tuplish())}, tuple()}}
        ]),
        list_wrap(fun(L) -> llists:keystore(Key, N, L, Replace) end, List) ==
            lists:keystore(Key, N, List, Replace)
    ).

prop_keytake() ->
    ?FORALL(
        {Key, {N, List}},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
            {1, {any(), {pos_integer(), list(tuplish())}}}
        ]),
        begin
            Result =
                case llists:keytake(Key, N, llists:from_list(List)) of
                    {value, Tuple, Iterator} ->
                        {value, Tuple, llists:to_list(Iterator)};
                    false ->
                        false
                end,
            Result == lists:keytake(Key, N, List)
        end
    ).

prop_map() ->
    ?FORALL(
        {Map, List},
        {function(1, any()), list()},
        begin
            list_wrap(fun(L) -> llists:map(Map, L) end, List) ==
                lists:map(Map, List)
        end
    ).

prop_merge_1() ->
    % I'm unable to replicate the `lists:merge` sorting behaviour for
    % invalid lists with more than 4 elements, so only test (valid)
    % sorted input here.
    ?FORALL(
        List,
        list(sorted(list())),
        begin
            Iterators = [llists:from_list(L) || L <- List],
            list_wrap(fun llists:merge/1, Iterators) ==
                lists:merge(List)
        end
    ).

prop_merge_2() ->
    ?FORALL(
        {List1, List2},
        {sortedish(list()), sortedish(list())},
        llists:to_list(
            llists:merge(
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:merge(List1, List2)
    ).

prop_merge_3() ->
    % Reverse of default ordering.
    Compare = fun(A, B) -> A > B end,
    ?FORALL(
        {List1, List2},
        {reversed(sortedish(list())), reversed(sortedish(list()))},
        llists:to_list(
            llists:merge(
                Compare,
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:merge(Compare, List1, List2)
    ).

prop_merge3() ->
    ?FORALL(
        {List1, List2, List3},
        {sortedish(list()), sortedish(list()), sortedish(list())},
        llists:to_list(
            llists:merge3(
                llists:from_list(List1),
                llists:from_list(List2),
                llists:from_list(List3)
            )
        ) ==
            lists:merge3(List1, List2, List3)
    ).

prop_nthtail() ->
    ?FORALL(
        {N, List},
        ?LET(L, list(), {integer(0, length(L)), L}),
        list_wrap(fun(L) -> llists:nthtail(N, L) end, List) ==
            lists:nthtail(N, List)
    ).

prop_reverse_1() ->
    ?FORALL(
        List,
        list(),
        list_wrap(fun llists:reverse/1, List) ==
            lists:reverse(List)
    ).

prop_reverse_2() ->
    ?FORALL(
        {List1, List2},
        {list(), list()},
        llists:to_list(
            llists:reverse(
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:reverse(List1, List2)
    ).

prop_sublist_2() ->
    ?FORALL(
        {List, Length},
        frequency([
            {5,
                ?LET(
                    List,
                    list(),
                    {List, integer(0, length(List))}
                )},
            {1, {list(), non_neg_integer()}}
        ]),
        list_wrap(fun(L) -> llists:sublist(L, Length) end, List) ==
            lists:sublist(List, Length)
    ).

prop_sublist_3() ->
    ?FORALL(
        {List, Start, Length},
        frequency([
            {5,
                ?LET(
                    List,
                    non_empty(list()),
                    {List, index(List), integer(0, length(List))}
                )},
            {1,
                ?LET(
                    List,
                    non_empty(list()),
                    {List, index(List), non_neg_integer()}
                )}
        ]),
        list_wrap(fun(L) -> llists:sublist(L, Start, Length) end, List) ==
            lists:sublist(List, Start, Length)
    ).

prop_takewhile() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        list_wrap(fun(L) -> llists:takewhile(Pred, L) end, List) ==
            lists:takewhile(Pred, List)
    ).

prop_partition() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        begin
            {S, NS} = llists:partition(Pred, llists:from_list(List)),
            {llists:to_list(S), llists:to_list(NS)} ==
                lists:partition(Pred, List)
        end
    ).

prop_sort_1() ->
    ?FORALL(
        List,
        list(),
        list_wrap(fun llists:sort/1, List) ==
            lists:sort(List)
    ).

prop_sort_2() ->
    % Reverse of default ordering.
    Compare = fun(A, B) -> A > B end,
    ?FORALL(
        List,
        list(),
        list_wrap(fun(L) -> llists:sort(Compare, L) end, List) ==
            lists:sort(Compare, List)
    ).

prop_split() ->
    ?FORALL(
        {N, List},
        ?LET(List, list(), {integer(0, length(List)), List}),
        begin
            {Before, After} = llists:split(N, llists:from_list(List)),
            {llists:to_list(Before), llists:to_list(After)} ==
                lists:split(N, List)
        end
    ).

prop_splitwith() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        begin
            {Before, After} = llists:splitwith(
                Pred,
                llists:from_list(List)
            ),
            {llists:to_list(Before), llists:to_list(After)} ==
                lists:splitwith(Pred, List)
        end
    ).

prop_subtract() ->
    ?FORALL(
        {List1, List2},
        frequency([
            {5,
                ?LET(
                    Elem,
                    any(),
                    {contains(Elem), contains(Elem)}
                )},
            {1, {list(), list()}}
        ]),
        llists:to_list(
            llists:subtract(
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:subtract(List1, List2)
    ).

% I'm unable to duplicate the undocumented behaviour for the unique
% merge functions in list. They require both sorted and unique input,
% and their behaviour when those are not present is fairly
% complicated. As such, the following properties all restrict
% themselves to only valid inputs.

prop_ukeymerge() ->
    ?FORALL(
        {N, List1, List2},
        ?LET(
            {N, List},
            key_list(),
            {N, ukeysorted(N, List), ukeysorted(N, key_list(any(), N))}
        ),
        llists:to_list(
            llists:ukeymerge(
                N,
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:ukeymerge(N, List1, List2)
    ).

prop_ukeysort() ->
    ?FORALL(
        {N, List1},
        key_list(),
        llists:to_list(llists:ukeysort(N, llists:from_list(List1))) ==
            lists:ukeysort(N, List1)
    ).

prop_umerge_1() ->
    ?FORALL(
        List,
        list(usorted(list())),
        begin
            Iterators = [llists:from_list(L) || L <- List],
            list_wrap(fun llists:umerge/1, Iterators) ==
                lists:umerge(List)
        end
    ).

prop_umerge_2() ->
    ?FORALL(
        {List1, List2},
        {usorted(list()), usorted(list())},
        llists:to_list(
            llists:umerge(
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:umerge(List1, List2)
    ).

prop_umerge_3() ->
    % Reverse of default ordering.
    Compare = fun(A, B) -> A > B end,
    ?FORALL(
        {List1, List2},
        {reversed(usorted(list())), reversed(usorted(list()))},
        llists:to_list(
            llists:umerge(
                Compare,
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:umerge(Compare, List1, List2)
    ).

prop_umerge3() ->
    ?FORALL(
        {List1, List2, List3},
        {usorted(list()), usorted(list()), usorted(list())},
        llists:to_list(
            llists:umerge3(
                llists:from_list(List1),
                llists:from_list(List2),
                llists:from_list(List3)
            )
        ) ==
            lists:umerge3(List1, List2, List3)
    ).

prop_uniq_1() ->
    ?FORALL(
        List,
        duplicates(),
        llists:to_list(
            llists:uniq(
                llists:from_list(List)
            )
        ) ==
            lists:uniq(List)
    ).

prop_uniq_2() ->
    Uniq = fun
        ({}) -> {};
        (T) when is_tuple(T) -> {element(1, T)};
        ([H | _Tail]) -> [H];
        (N) when is_float(N) -> float(trunc(N));
        (X) -> X
    end,
    ?FORALL(
        List,
        duplicates(),
        llists:to_list(
            llists:uniq(
                Uniq,
                llists:from_list(List)
            )
        ) ==
            lists:uniq(Uniq, List)
    ).

prop_unzip() ->
    ?FORALL(
        List,
        list({any(), any()}),
        begin
            {First, Second} = llists:unzip(llists:from_list(List)),
            {llists:to_list(First), llists:to_list(Second)} ==
                lists:unzip(List)
        end
    ).

prop_unzip3() ->
    ?FORALL(
        List,
        list({any(), any(), any()}),
        begin
            {First, Second, Third} = llists:unzip3(llists:from_list(List)),
            {llists:to_list(First), llists:to_list(Second), llists:to_list(Third)} ==
                lists:unzip3(List)
        end
    ).

prop_usort_1() ->
    ?FORALL(
        List,
        list(),
        list_wrap(fun llists:usort/1, List) ==
            lists:usort(List)
    ).

prop_usort_2() ->
    % Reverse of default ordering.
    Compare = fun(A, B) -> A > B end,
    ?FORALL(
        List,
        list(),
        list_wrap(fun(L) -> llists:usort(Compare, L) end, List) ==
            lists:usort(Compare, List)
    ).

prop_zip() ->
    ?FORALL(
        {List1, List2},
        ?LET(L, list(), {L, list(any(), length(L))}),
        llists:to_list(
            llists:zip(
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:zip(List1, List2)
    ).

prop_zip3() ->
    ?FORALL(
        {List1, List2, List3},
        ?LET(
            L,
            list(),
            {L, list(any(), length(L)), list(any(), length(L))}
        ),
        llists:to_list(
            llists:zip3(
                llists:from_list(List1),
                llists:from_list(List2),
                llists:from_list(List3)
            )
        ) ==
            lists:zip3(List1, List2, List3)
    ).

prop_zipwith() ->
    ?FORALL(
        {Combine, List1, List2},
        ?LET(L, list(), {function(2, any()), L, list(any(), length(L))}),
        llists:to_list(
            llists:zipwith(
                Combine,
                llists:from_list(List1),
                llists:from_list(List2)
            )
        ) ==
            lists:zipwith(Combine, List1, List2)
    ).

prop_zipwith3() ->
    ?FORALL(
        {Combine, List1, List2, List3},
        ?LET(
            L,
            list(),
            {function(3, any()), L, list(any(), length(L)), list(any(), length(L))}
        ),
        llists:to_list(
            llists:zipwith3(
                Combine,
                llists:from_list(List1),
                llists:from_list(List2),
                llists:from_list(List3)
            )
        ) ==
            lists:zipwith3(Combine, List1, List2, List3)
    ).

prop_length() ->
    ?FORALL(
        List,
        list(),
        llists:length(llists:from_list(List)) ==
            erlang:length(List)
    ).

prop_all() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        llists:all(Pred, llists:from_list(List)) ==
            lists:all(Pred, List)
    ).

prop_any() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        llists:any(Pred, llists:from_list(List)) ==
            lists:any(Pred, List)
    ).

prop_concat() ->
    ?FORALL(
        List,
        list(union([atom(), integer(), float(), string()])),
        llists:concat(llists:from_list(List)) ==
            lists:concat(List)
    ).

prop_foldl() ->
    ?FORALL(
        {Fold, Acc0, List},
        {function(2, any()), any(), list()},
        llists:foldl(Fold, Acc0, llists:from_list(List)) ==
            lists:foldl(Fold, Acc0, List)
    ).

prop_foldr() ->
    ?FORALL(
        {Fold, Acc0, List},
        {function(2, any()), any(), list()},
        llists:foldr(Fold, Acc0, llists:from_list(List)) ==
            lists:foldr(Fold, Acc0, List)
    ).

prop_foreach() ->
    ?FORALL(
        {For, List},
        {function(1, any()), list()},
        llists:foreach(For, llists:from_list(List)) ==
            lists:foreach(For, List)
    ).

prop_keyfind() ->
    ?FORALL(
        {Key, {N, List}},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
            {1, {any(), {pos_integer(), list(tuplish())}}}
        ]),
        llists:keyfind(Key, N, llists:from_list(List)) ==
            lists:keyfind(Key, N, List)
    ).

prop_keymember() ->
    ?FORALL(
        {Key, {N, List}},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
            {1, {any(), {pos_integer(), list(tuplish())}}}
        ]),
        llists:keymember(Key, N, llists:from_list(List)) ==
            lists:keymember(Key, N, List)
    ).

prop_keysearch() ->
    ?FORALL(
        {Key, {N, List}},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
            {1, {any(), {pos_integer(), list(tuplish())}}}
        ]),
        llists:keysearch(Key, N, llists:from_list(List)) ==
            lists:keysearch(Key, N, List)
    ).

prop_last() ->
    ?FORALL(
        List,
        non_empty(list()),
        llists:last(llists:from_list(List)) ==
            lists:last(List)
    ).

prop_mapfoldl() ->
    ?FORALL(
        {MapFold, Acc0, List},
        {function(2, {any(), any()}), any(), list()},
        begin
            {Fold, Acc} = llists:mapfoldl(
                MapFold,
                Acc0,
                llists:from_list(List)
            ),
            {llists:to_list(Fold), Acc} ==
                lists:mapfoldl(MapFold, Acc0, List)
        end
    ).

prop_mapfoldr() ->
    ?FORALL(
        {MapFold, Acc0, List},
        {function(2, {any(), any()}), any(), list()},
        begin
            {Fold, Acc} = llists:mapfoldr(
                MapFold,
                Acc0,
                llists:from_list(List)
            ),
            {llists:to_list(Fold), Acc} ==
                lists:mapfoldr(MapFold, Acc0, List)
        end
    ).

prop_max() ->
    ?FORALL(
        List,
        non_empty(list()),
        llists:max(llists:from_list(List)) ==
            lists:max(List)
    ).

prop_member() ->
    ?FORALL(
        {Elem, List},
        frequency([
            {5, ?LET(Elem, any(), {Elem, contains(Elem)})},
            {1, {any(), list()}}
        ]),
        llists:member(Elem, llists:from_list(List)) ==
            lists:member(Elem, List)
    ).

prop_min() ->
    ?FORALL(
        List,
        non_empty(list()),
        llists:min(llists:from_list(List)) ==
            lists:min(List)
    ).

prop_nth() ->
    ?FORALL(
        {N, List},
        ?LET(L, non_empty(list()), {integer(1, length(L)), L}),
        llists:nth(N, llists:from_list(List)) ==
            lists:nth(N, List)
    ).

prop_prefix() ->
    ?FORALL(
        {List1, List2},
        frequency([
            {1,
                ?LET(
                    {List1, List2},
                    {list(), list()},
                    {List1, List1 ++ List2}
                )},
            {1, {list(), list()}}
        ]),
        llists:prefix(llists:from_list(List1), llists:from_list(List2)) ==
            lists:prefix(List1, List2)
    ).

prop_search() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        llists:search(Pred, llists:from_list(List)) ==
            lists:search(Pred, List)
    ).

prop_suffix() ->
    ?FORALL(
        {List1, List2},
        frequency([
            {1,
                ?LET(
                    {List1, List2},
                    {list(), list()},
                    {List1, List2 ++ List1}
                )},
            {1, {list(), list()}}
        ]),
        llists:suffix(llists:from_list(List1), llists:from_list(List2)) ==
            lists:suffix(List1, List2)
    ).

prop_sum() ->
    ?FORALL(
        List,
        list(number()),
        llists:sum(llists:from_list(List)) ==
            lists:sum(List)
    ).

%%%===================================================================
%%% Generators
%%%===================================================================

iterator() ->
    iterator(any()).

iterator(Of) ->
    ?LET(List, list(Of), llists:from_list(List)).

range(Start) ->
    ?LET(From, integer(Start, inf), {From, integer(From, inf)}).

sign(Of) when Of >= 0 ->
    pos_integer();
sign(Of) when Of < 0 ->
    neg_integer().

list(Of, Length) ->
    ?LET(L, Length, lists:duplicate(L, Of)).

small_list() ->
    small_list(any()).

small_list(Of) ->
    ?SIZED(Size, resize(Size div 4, list(Of))).

deep_list() ->
    list(
        frequency([
            {1, small_list(union([any(), small_list()]))},
            {8, small_list()},
            {4, any()}
        ])
    ).

duplicates() ->
    frequency([
        {1, ?LET(List, list(), List ++ List)},
        {1, ?LET(List, list(), List)}
    ]).

key_list() ->
    key_list(any()).

key_list(Of) ->
    % Return a valid key index and a list of tuples of the same size.
    ?LET(
        Length,
        integer(1, 10),
        {Length, key_list(Of, Length)}
    ).

key_list(Of, Length) ->
    ?LET(
        L,
        Length,
        list(list_to_tuple(lists:duplicate(L, Of)))
    ).

tuplish() ->
    frequency([
        {5, tuple()},
        {1, any()}
    ]).

index(List) ->
    ?LET(L, List, integer(1, length(L))).

contains(Elem) ->
    frequency([
        {5,
            ?LET(
                {Before, After},
                {list(), list()},
                Before ++ [Elem] ++ After
            )},
        {1,
            ?LET(
                {Before, Between, After},
                {list(), list(), list()},
                Before ++ [Elem] ++ Between ++ [Elem] ++ After
            )}
    ]).

contains_key(Elem) ->
    ?LET(
        Length,
        integer(1, 10),
        ?LET(
            N,
            integer(1, Length),
            begin
                Tuple = list_to_tuple(lists:duplicate(Length, any())),
                WithElem = [setelement(N, Tuple, Elem)],
                ?LET(
                    {Before, Between, After},
                    {list(Tuple), list(Tuple), list(Tuple)},
                    frequency([
                        {5, {N, Before ++ WithElem ++ After}},
                        {1, {N, Before ++ WithElem ++ Between ++ WithElem ++ After}}
                    ])
                )
            end
        )
    ).

reversed(List) ->
    ?LET(L, List, lists:reverse(L)).

sorted(List) ->
    ?LET(L, List, lists:sort(L)).

sortedish(List) ->
    frequency([
        {10, sorted(List)},
        {1, List}
    ]).

keysorted(N, List) ->
    ?LET(L, List, lists:keysort(N, L)).

keysortedish(N, List) ->
    frequency([
        {10, keysorted(N, List)},
        {1, List}
    ]).

usorted(List) ->
    ?LET(L, List, lists:usort(L)).

ukeysorted(N, List) ->
    ?LET(L, List, lists:ukeysort(N, L)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

list_wrap(Fun, List) ->
    llists:to_list(Fun(llists:from_list(List))).

from_deep_list(List) ->
    llists:from_list(from_deep_list_inner(List)).

from_deep_list_inner([]) ->
    [];
from_deep_list_inner([Head | Tail]) when is_list(Head) ->
    [from_deep_list(Head) | from_deep_list_inner(Tail)];
from_deep_list_inner([Head | Tail]) ->
    [Head | from_deep_list_inner(Tail)].
