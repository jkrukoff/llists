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
    ?FORALL(List,
            list(),
            list_wrap(fun (L) -> L end, List) == List).

prop_from_map() ->
    ?FORALL(Map,
            map(any(), any()),
            llists:to_map(llists:from_map(Map)) == Map).

prop_duplicate() ->
    ?FORALL({Count, Term},
            {integer(0, inf), any()},
            llists:to_list(llists:duplicate(Count, Term)) ==
            lists:duplicate(Count, Term)).

prop_seq_2() ->
    ?FORALL({From, To},
            range(inf),
            llists:to_list(llists:seq(From, To)) ==
            lists:seq(From, To)).

prop_seq_3() ->
    ?FORALL({From, To, Incr},
            ?LET({From, To}, range(inf), {From, To, sign(To - From)}),
            llists:to_list(llists:seq(From, To, Incr)) ==
            lists:seq(From, To, Incr)).

prop_next() ->
    ?FORALL(List,
            list(),
            begin
                case llists:next(llists:from_list(List)) of
                    [] ->
                        List == [];
                    [Elem | Iterator] ->
                        (Elem == hd(List)) and llists:is_iterator(Iterator)
                end
            end).

prop_hd() ->
    ?FORALL(List,
            non_empty(list()),
            llists:hd(llists:from_list(List)) == hd(List)).

prop_tl() ->
    ?FORALL(List,
            non_empty(list()),
            llists:is_iterator(llists:tl(llists:from_list(List)))).

prop_append_1() ->
    ?FORALL(List,
            list(small_list()),
            list_wrap(fun llists:append/1, List) == lists:append(List)).

prop_append_2() ->
    ?FORALL({List1, List2},
            {list(), list()},
            llists:to_list(
              llists:append(
                llists:from_list(List1), llists:from_list(List2))) ==
            lists:append(List1, List2)).

prop_delete() ->
    ?FORALL({Elem, List},
            frequency([{5, ?LET(Elem, any(), {Elem, contains(Elem)})},
                       {1, {any(), list()}}]),
            list_wrap(fun (L) -> llists:delete(Elem, L) end, List) ==
            lists:delete(Elem, List)).

prop_droplast() ->
    ?FORALL(List,
            non_empty(list()),
            list_wrap(fun llists:droplast/1, List) == lists:droplast(List)).

prop_dropwhile() ->
    ?FORALL({Pred, List},
            {function(1, boolean()), list()},
            begin
                list_wrap(fun (L) -> llists:dropwhile(Pred, L) end, List) ==
                lists:dropwhile(Pred, List)
            end).

prop_filter() ->
    ?FORALL({Pred, List},
            {function(1, boolean()), list()},
            begin
                list_wrap(fun (L) -> llists:filter(Pred, L) end, List) ==
                lists:filter(Pred, List)
            end).

prop_filtermap() ->
    ?FORALL({FilterMap, List},
            {function(1, union([boolean(), {true, any()}])), list(integer())},
            begin
                list_wrap(fun (L) -> llists:filtermap(FilterMap, L) end,
                          List) ==
                lists:filtermap(FilterMap, List)
            end).

prop_flatlength() ->
    ?FORALL(List,
            deep_list(),
            llists:flatlength(from_deep_list(List)) ==
            lists:flatlength(List)).

prop_flatmap() ->
    ?FORALL({Map, List},
            {function(1, small_list()), list()},
            begin
                IteratorMap = fun (E) -> llists:from_list(Map(E)) end,
                list_wrap(fun (L) -> llists:flatmap(IteratorMap, L) end,
                          List) ==
                lists:flatmap(Map, List)
            end).

prop_flatten_1() ->
    ?FORALL(List,
            deep_list(),
            llists:to_list(llists:flatten(from_deep_list(List))) ==
            lists:flatten(List)).

prop_flatten_2() ->
    ?FORALL({List, Tail},
            {deep_list(), list()},
            llists:to_list(llists:flatten(from_deep_list(List),
                                          llists:from_list(Tail))) ==
            lists:flatten(List, Tail)).

prop_join() ->
    ?FORALL({Sep, List},
            {any(), list()},
            list_wrap(fun (L) -> llists:join(Sep, L) end, List) ==
            lists:join(Sep, List)).

prop_keydelete() ->
    ?FORALL({Key, {N, List}},
            frequency([{5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
                       {1, {any(), {pos_integer(), list(tuplish())}}}]),
            list_wrap(fun (L) -> llists:keydelete(Key, N, L) end, List) ==
            lists:keydelete(Key, N, List)).

prop_keymap() ->
    ?FORALL({Map, {N, List}},
            {function(1, any()), key_list()},
            begin
                list_wrap(fun (L) -> llists:keymap(Map, N, L) end, List) ==
                lists:keymap(Map, N, List)
            end).

prop_keymerge() ->
    ?FORALL({N, List1, List2},
            ?LET({N, List},
                 key_list(),
                 {N,
                  keysortedish(N, List),
                  keysortedish(N, key_list(any(), N))}),
            llists:to_list(llists:keymerge(N,
                                           llists:from_list(List1),
                                           llists:from_list(List2))) ==
            lists:keymerge(N, List1, List2)).

prop_keyreplace() ->
    ?FORALL({Key, {N, List}, Replace},
            frequency([{5, ?LET(Elem, any(), {Elem, contains_key(Elem), tuple()})},
                       {1, {any(), {pos_integer(), list(tuplish())}, tuple()}}]),
            list_wrap(fun (L) -> llists:keyreplace(Key, N, L, Replace) end, List) ==
            lists:keyreplace(Key, N, List, Replace)).

prop_keysort() ->
    ?FORALL({N, List},
            key_list(),
            list_wrap(fun (L) -> llists:keysort(N, L) end, List) ==
            lists:keysort(N, List)).

prop_keystore() ->
    ?FORALL({Key, {N, List}, Replace},
            frequency([{5, ?LET(Elem, any(), {Elem, contains_key(Elem), tuple()})},
                       {1, {any(), {pos_integer(), list(tuplish())}, tuple()}}]),
            list_wrap(fun (L) -> llists:keystore(Key, N, L, Replace) end, List) ==
            lists:keystore(Key, N, List, Replace)).

prop_keytake() ->
    ?FORALL({Key, {N, List}},
            frequency([{5, ?LET(Elem, any(), {Elem, contains_key(Elem)})},
                       {1, {any(), {pos_integer(), list(tuplish())}}}]),
            begin
                Result = case llists:keytake(Key, N, llists:from_list(List)) of
                             {value, Tuple, Iterator} ->
                                 {value, Tuple, llists:to_list(Iterator)};
                             false ->
                                 false
                         end,
                Result == lists:keytake(Key, N, List)
            end).

prop_map() ->
    ?FORALL({Map, List},
            {function(1, any()), list()},
            begin
                list_wrap(fun (L) -> llists:map(Map, L) end, List) ==
                lists:map(Map, List)
            end).

prop_merge_1() ->
    % I'm unable to replicate the `lists:merge` sorting behaviour for
    % invalid lists with more than 4 elements, so only test (valid)
    % sorted input here.
    ?FORALL(List,
            list(sorted(list())),
            begin
                Iterators = [llists:from_list(L) || L <- List],
                list_wrap(fun llists:merge/1, Iterators) ==
                lists:merge(List)
            end).

prop_merge_2() ->
    ?FORALL({List1, List2},
            {sortedish(list()), sortedish(list())},
            llists:to_list(llists:merge(llists:from_list(List1),
                                        llists:from_list(List2))) ==
            lists:merge(List1, List2)).

prop_merge_3() ->
    % Reverse of default ordering.
    Compare = fun (A, B) -> A > B end,
    ?FORALL({List1, List2},
            {reversed(sortedish(list())), reversed(sortedish(list()))},
            llists:to_list(llists:merge(Compare,
                                        llists:from_list(List1),
                                        llists:from_list(List2))) ==
            lists:merge(Compare, List1, List2)).

prop_merge3() ->
    ?FORALL({List1, List2, List3},
            {sortedish(list()), sortedish(list()), sortedish(list())},
            llists:to_list(llists:merge3(llists:from_list(List1),
                                         llists:from_list(List2),
                                         llists:from_list(List3))) ==
            lists:merge3(List1, List2, List3)).

prop_nthtail() ->
    ?FORALL({N, List},
            ?LET(Length,
                 non_neg_integer(),
                 {integer(0, Length), lists:duplicate(Length, any())}),
            list_wrap(fun (L) -> llists:nthtail(N, L) end, List) ==
            lists:nthtail(N, List)).

prop_reverse_1() ->
    ?FORALL(List,
            list(),
            list_wrap(fun llists:reverse/1, List) ==
            lists:reverse(List)).

prop_reverse_2() ->
    ?FORALL({List1, List2},
            {list(), list()},
            llists:to_list(llists:reverse(llists:from_list(List1),
                                          llists:from_list(List2))) ==
            lists:reverse(List1, List2)).

prop_sublist_2() ->
    ?FORALL({List, Length},
            frequency([{5, ?LET(List,
                                list(),
                                {List, integer(0, length(List))})},
                       {1, {list(), non_neg_integer()}}]),
            list_wrap(fun (L) -> llists:sublist(L, Length) end, List) ==
            lists:sublist(List, Length)).

prop_sublist_3() ->
    ?FORALL({List, Start, Length},
            frequency([{5, ?LET(List,
                                non_empty(list()),
                                {List, index(List), integer(0, length(List))})},
                       {1, ?LET(List,
                                non_empty(list()),
                                {List, index(List), non_neg_integer()})}]),
            list_wrap(fun (L) -> llists:sublist(L, Start, Length) end, List) ==
            lists:sublist(List, Start, Length)).

prop_takewhile() ->
    ?FORALL({Pred, List},
            {function(1, boolean()), list()},
            list_wrap(fun (L) -> llists:takewhile(Pred, L) end, List) ==
            lists:takewhile(Pred, List)).

prop_partition() ->
    ?FORALL({Pred, List},
            {function(1, boolean()), list()},
            begin
                {S, NS} = llists:partition(Pred, llists:from_list(List)),
                {llists:to_list(S), llists:to_list(NS)} ==
                lists:partition(Pred, List)
            end).

prop_split() ->
    ?FORALL({N, List},
            ?LET(List, list(), {integer(0, length(List)), List}),
            begin
                {Before, After} = llists:split(N, llists:from_list(List)),
                {llists:to_list(Before), llists:to_list(After)} ==
                lists:split(N, List)
            end).

prop_splitwith() ->
    ?FORALL({Pred, List},
            {function(1, boolean()), list()},
            begin
                {Before, After} = llists:splitwith(Pred,
                                                   llists:from_list(List)),
                {llists:to_list(Before), llists:to_list(After)} ==
                lists:splitwith(Pred, List)
            end).

prop_subtract() ->
    ?FORALL({List1, List2},
            frequency([{5, ?LET(Elem,
                                any(),
                                {contains(Elem), contains(Elem)})},
                       {1, {list(), list()}}]),
            llists:to_list(llists:subtract(llists:from_list(List1),
                                           llists:from_list(List2))) ==
            lists:subtract(List1, List2)).

prop_ukeymerge() ->
    ?FORALL({N, List1, List2},
            ?LET({N, List},
                 key_list(),
                 {N,
                  keysortedish(N, List),
                  keysortedish(N, key_list(any(), N))}),
            llists:to_list(llists:ukeymerge(N,
                                           llists:from_list(List1),
                                           llists:from_list(List2))) ==
            lists:ukeymerge(N, List1, List2)).

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

small_list() ->
    small_list(any()).

small_list(Of) ->
    ?SIZED(Size, resize(Size div 4, list(Of))).

deep_list() ->
    list(frequency([{1, small_list(union([any(), small_list()]))},
                    {8, small_list()},
                    {4, any()}])).

index_list() ->
    index_list(any()).

index_list(Of) ->
    % Return a random index into a list and a list long enough to
    % contain that index.
    ?LET(Length,
         non_neg_integer(),
         {integer(1, Length), resize(Length, list(Of))}).

key_list() ->
    key_list(any()).

key_list(Of) ->
    % Return a valid key index and a list of tuples of the same size.
    ?LET(Length,
         integer(1, 10),
         {Length, key_list(Of, Length)}).

key_list(Of, Length) ->
    list(list_to_tuple(lists:duplicate(Length, Of))).

tuplish() ->
    frequency([{5, tuple()},
               {1, any()}]).

index(List) ->
    ?LET(L, List, integer(1, length(L))).

contains(Elem) ->
    frequency([{5, ?LET({Before, After},
                        {list(), list()},
                        Before ++ [Elem] ++ After)},
               {1, ?LET({Before, Between, After},
                        {list(), list(), list()},
                        Before ++ [Elem] ++ Between ++ [Elem] ++ After)}]).

contains_key(Elem) ->
    ?LET(Length,
         integer(1, 10),
         ?LET(N,
              integer(1, Length),
              begin
                  Tuple = list_to_tuple(lists:duplicate(Length, any())),
                  WithElem = [setelement(N, Tuple, Elem)],
                  ?LET({Before, Between, After},
                       {list(Tuple), list(Tuple), list(Tuple)},
                       frequency([{5, {N, Before ++ WithElem ++ After}},
                                  {1, {N, Before ++ WithElem ++ Between ++ WithElem ++ After}}]))
              end)).

reversed(List) ->
    ?LET(L, List, lists:reverse(L)).

sorted(List) ->
    ?LET(L, List, lists:sort(L)).

sortedish(List) ->
    frequency([{10, sorted(List)},
               {1, List}]).

keysorted(N, List) ->
    ?LET(L, List, lists:keysort(N, L)).

keysortedish(N, List) ->
    frequency([{10, keysorted(N, List)},
               {1, List}]).

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
