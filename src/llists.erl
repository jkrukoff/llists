%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llists).

-record(iterator, {next}).

% -type next() :: next(any()).
-type next(Over) :: fun((Acc :: accumulator()) -> {Elem :: Over, NewAcc :: accumulator()} | none).
-type iterator() :: iterator(any()).
-opaque iterator(Over) :: #iterator{next :: fun(() -> lazy_list(Over))}.
-type accumulator() :: any().
% -type lazy_list() :: lazy_list(any()).
-type lazy_list(Over) :: nonempty_improper_list(Over, iterator(Over)) | [].

% -type predicate() :: predicate(any()).
-type predicate(Elem) :: fun((Elem) -> boolean()).
-type fold(Elem, AccIn, AccOut) :: fun((Elem, AccIn) -> AccOut).

%% API
-export([% Iterator construction.
         is_iterator/1,
         from_list/1,
         unfold/2,
         duplicate/2,
         seq/2,
         seq/3,
         % Iterator utilities.
         append/1,
         append/2,
         concat/1,
         delete/2,
         droplast/1,
         dropwhile/2,
         filter/2,
         filtermap/2,
         flatlength/1,
         flatmap/2,
         flatten/1,
         flatten/2,
         join/2,
         map/2,
         merge/1,
         merge/2,
         merge/3,
         merge3/3,
         nthtail/2,
         reverse/1,
         reverse/2,
         sublist/2,
         sublist/3,
         takewhile/2,
         partition/2,
         split/2,
         splitwith/2,
         subtract/2,
         umerge/1,
         umerge/2,
         umerge/3,
         % Iterator evaluation.
         next/1,
         to_list/1,
         hd/1,
         length/1,
         tl/1,
         all/2,
         any/2,
         foldl/3,
         foldr/3,
         foreach/2,
         last/1,
         mapfoldl/3,
         mapfoldr/3,
         max/1,
         member/2,
         min/1,
         nth/2,
         prefix/2,
         sort/1,
         sort/2,
         search/2,
         suffix/2,
         sum/1]).

-export_type([next/1,
              iterator/1,
              lazy_list/1]).

-compile({no_auto_import,
          [hd/1,
           max/1,
           min/1,
           length/1,
           tl/1]}).

%%%===================================================================
%%% API - Iterator Construction
%%%===================================================================

-spec is_iterator(any()) -> boolean().
is_iterator(#iterator{}) ->
    true;
is_iterator(_) ->
    false.

-spec from_list(list(Elem)) -> iterator(Elem).
from_list(List) ->
    unfold(fun ([]) -> none;
               ([Head | Tail]) -> {Head, Tail}
           end,
           List).

-spec unfold(Fun :: next(Elem), Acc :: accumulator()) -> iterator(Elem).
unfold(Fun, Acc) ->
    new(fun () ->
                case Fun(Acc) of
                    {Elem, NewAcc} ->
                        [Elem | unfold(Fun, NewAcc)];
                    none ->
                        []
                end
        end).

%% @doc
%% Returns an iterator containing `N' copies of term `Elem'.
%% @end
-spec duplicate(infinity, Elem) -> iterator(Elem);
               (N :: non_neg_integer(), Elem) -> iterator(Elem).
duplicate(infinity, Elem) ->
    new(fun () ->
                [Elem | duplicate(infinity, Elem)]
        end);
duplicate(N, Elem) when is_integer(N), N >= 0 ->
    unfold(fun (0) -> none;
               (Count) -> {Elem, Count - 1}
           end,
           N).

%% @see seq/3
-spec seq(From :: integer(), To :: integer()) -> iterator(integer()).
seq(From, To) ->
    seq(From, To, 1).

%% @doc
%% Returns an iterator over a sequence of integers that starts with
%% `From' and contains the successive results of adding `Incr' to the
%% previous element, until `To' is reached or passed (in the latter
%% case, `To' is not an element of the sequence). `Incr' defaults to
%% 1.
%%
%% Failures:
%% <ul>
%% <li>If `To < From - Incr' and `Incr > 0'.</li>
%% <li>If `To > From - Incr' and `Incr < 0'.</li>
%% <li>If `Incr =:= 0' and `From =/= To'.</li>
%% </ul>
%% The following equalities hold for all sequences:
%% ```
%% length(lists:seq(From, To)) =:= To - From + 1
%% length(lists:seq(From, To, Incr)) =:= (To - From + Incr) div Incr
%% '''
%% @end
-spec seq(From :: integer(),
          To :: infinity | '-infinity',
          Incr :: integer()) -> iterator(integer());
         (From :: integer(),
          To :: integer(),
          Incr :: integer()) -> iterator(integer()).
seq(From, infinity, Incr) when is_integer(From),
                               is_integer(Incr),
                               Incr > 0 ->
    unfold(fun (Acc) -> {Acc, Acc + Incr} end, From);
seq(From, '-infinity', Incr) when is_integer(From),
                                  is_integer(Incr),
                                  Incr < 0 ->
    unfold(fun (Acc) -> {Acc, Acc + Incr} end, From);
seq(From, To, Incr) when is_integer(From),
                         is_integer(To),
                         is_integer(Incr),
                         Incr > 0,
                         From - Incr =< To ->
    unfold(fun (Acc) when Acc > To -> none;
               (Acc) -> {Acc, Acc + Incr}
           end,
           From);
seq(From, To, Incr) when is_integer(From),
                         is_integer(To),
                         is_integer(Incr),
                         Incr < 0,
                         From - Incr >= To ->
    unfold(fun (Acc) when Acc < To -> none;
               (Acc) -> {Acc, Acc + Incr}
           end,
           From);
seq(From, From, 0) when is_integer(From) ->
    from_list([From]).

%%%===================================================================
%%% API - Iterator Utilities
%%%===================================================================

%% @doc
%% Returns an iterator in which all the subiterators of
%% `IteratorOfIterators' have been appended.
%% @end
-spec append(IteratorOfIterators :: iterator(iterator())) -> iterator().
append(#iterator{} = Iterator) ->
    unfold(fun Next({[], []}) ->
                   none;
               Next({#iterator{} = HeadIterator, IofI}) ->
                   Next({next(HeadIterator), IofI});
               Next({[Head | HeadIterator], IofI}) ->
                   {Head, {HeadIterator, IofI}};
               Next({[], #iterator{} = IofI}) ->
                   Next({[], next(IofI)});
               Next({[], [Head | IofI]}) ->
                   Next({Head, IofI})
           end,
           {[], Iterator}).

%% @doc
%% Returns a new iterator `Iterator3', which is made from the elements
%% of `Iterator1' followed by the elements of `Iterator2'.
%% @end
-spec append(Iterator1 :: iterator(), Iterator2 :: iterator()) ->
    Iterator3 :: iterator().
append(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    append(from_list([Iterator1, Iterator2])).

%% @doc
%% Concatenates the text representation of the elements of `Iterator'.
%% The elements of `Iterator' can be atoms, integers, floats, or
%% strings. The iterator will be fully evaluated, infinite iterators
%% will never return.
%% @end
-spec concat(Iterator :: iterator(atom() | integer() | float() | string())) ->
    string().
concat(#iterator{} = Iterator) ->
    lists:concat(to_list(Iterator)).

%% @doc
%% Returns a copy of `Iterator' where the first element matching
%% `Elem' is deleted, if there is such an element.
%% @end
-spec delete(Elem, iterator(Elem)) -> iterator(Elem).
delete(Elem, #iterator{} = Iterator) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | NextIterator] ->
                        next(NextIterator);
                    [Other | NextIterator] ->
                        [Other | delete(Elem, NextIterator)];
                    [] ->
                        []
                end
        end).

%% @doc
%% Drops the last element of a `Iterator'. The `Iterator' is to be non-empty,
%% otherwise the function crashes with a `function_clause'.
%%
%% Evaluates one element further in the iterator than the current
%% value.
%% @end
-spec droplast(Iterator :: iterator(Elem)) -> iterator(Elem).
droplast(#iterator{} = Iterator) ->
    unfold(fun ([Elem | #iterator{} = FoldIterator]) ->
                   case next(FoldIterator) of
                       [] ->
                           none;
                       [NextElem | #iterator{} = NextIterator] ->
                           {Elem, [NextElem | NextIterator]}
                   end;
               ([Elem | Tail]) ->
                   {Elem, Tail}
           end,
           next(Iterator)).

%% @doc
%% Drops elements `Elem' from `Iterator' while `Pred(Elem)' returns
%% true and returns the remaining iterator.
%% @end
-spec dropwhile(Pred :: predicate(Elem), Iterator :: iterator(Elem)) ->
    iterator(Elem).
dropwhile(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | NextIterator] ->
                        case Pred(Elem) of
                            true ->
                                next(dropwhile(Pred, NextIterator));
                            false ->
                                [Elem | NextIterator]
                        end;
                    [] ->
                        []
                end
        end).

%% @doc
%% `Filtered' is an iterator of all elements `Elem' in `Iterator' for
%% which `Pred(Elem)' returns `true'.
%% @end
-spec filter(Pred :: fun((Elem) -> boolean()), Iterator :: iterator(Elem)) ->
    iterator(Elem).
filter(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | NextIterator] ->
                        case Pred(Elem) of
                            true ->
                                [Elem | filter(Pred, NextIterator)];
                            false ->
                                next(filter(Pred, NextIterator))
                        end;
                    [] ->
                        []
                end
        end).

%% @doc
%% Calls `Fun(Elem)' on successive elements `Elem' of `Iterator'.
%% `Fun/1' must return either a Boolean or a tuple `{true, Value}'.
%% The function returns the list of elements for which `Fun' returns a
%% new value, where a value of `true' is synonymous with `{true,
%% Elem}'.
%%
%% That is, filtermap behaves as if it had been defined as follows,
%% except that the iterator is not fully evaluated before elements are
%% returned:
%%
%% ```
%% filtermap(Fun, Iterator) ->
%%     llists:foldr(fun(Elem, Acc) ->
%%                         case Fun(Elem) of
%%                             false -> Acc;
%%                             true -> [Elem|Acc];
%%                             {true,Value} -> [Value|Acc]
%%                         end
%%                  end, [], Iterator).
%% '''
%%
%% Example:
%%
%% ```
%% > llists:to_list(
%% >  llists:filtermap(
%% >   fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end,
%% >   llists:seq(1, 5))).
%% [1,2]
%% '''
%% @end
-spec filtermap(Fun :: fun((Elem) -> boolean() | {true, Value :: any()}), iterator(Elem)) ->
    iterator().
filtermap(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | NextIterator] ->
                        case Fun(Elem) of
                            false ->
                                next(filtermap(Fun, NextIterator));
                            true ->
                                [Elem | filtermap(Fun, NextIterator)];
                            {true, Value} ->
                                [Value | filtermap(Fun, NextIterator)]
                        end;
                    [] ->
                        []
                end
        end).

%% @doc
%% Equivalent to `length(flatten(DeepIterator))'.
%% @end
-spec flatlength(DeepIterator :: iterator(any() | iterator())) -> non_neg_integer().
flatlength(#iterator{} = DeepIterator) ->
    length(flatten(DeepIterator)).

%% @doc
%% Takes a function from `A's to iterators of `B's, and an iterator of
%% `A's (`Iterator') and produces an iterator of `B's by applying the
%% function to every element in `Iterator' and appending the resulting
%% iterators.
%%
%% That is, flatmap behaves as if it had been defined as follows:
%%
%% ```
%% llists:flatmap(Fun, Iterator) ->
%%     llists:append(llists:map(Fun, Iterator)).
%% '''
%%
%% Example:
%%
%% ```
%% > llists:to_list(
%% >  llists:flatmap(
%% >   fun(X)->llists:from_list([X,X]) end,
%% >   llists:from_list([a,b,c]))).
%% [a,a,b,b,c,c]
%% '''
-spec flatmap(Fun :: fun((A) -> iterator(B)), Iterator :: iterator(A)) -> iterator(B).
flatmap(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    llists:append(llists:map(Fun, Iterator)).

%% @doc
%% Returns a flattened version of `DeepIterator'.
%% @end
-spec flatten(DeepIterator :: iterator(any() | iterator())) -> iterator().
flatten(#iterator{} = DeepIterator) ->
    unfold(fun Next([]) ->
                   none;
               Next([#iterator{} = Iterator | Tail]) ->
                   Next([next(Iterator) | Tail]);
               Next([[] | Tail]) ->
                   Next(Tail);
               Next([[#iterator{} = NestedIterator | #iterator{} = Iterator] | Tail]) ->
                   Next([NestedIterator, Iterator | Tail]);
               Next([[Elem | #iterator{} = Iterator] | Tail]) ->
                   {Elem, [Iterator | Tail]}
           end,
           [DeepIterator]).

%% @doc
%% Returns a flattened version of `DeepIterator' with tail `Tail'
%% appended.
%% @end
-spec flatten(DeepIterator :: iterator(any() | iterator()),
              Tail :: iterator()) -> iterator().
flatten(#iterator{} = DeepIterator, #iterator{} = Tail) ->
    append(flatten(DeepIterator), Tail).

%% @doc
%% Inserts `Sep' between each element in `Iterator'. Has no effect on
%% an empty iterator or on a singleton iterator. For example:
%%
%% ```
%% > llists:to_list(llists:join(x, llists:from_list([a,b,c]))).
%% [a,x,b,x,c]
%% > llists:to_list(lists:join(x, llists:from_list([a]))).
%% [a]
%% > llists:to_list(lists:join(x, llists:from_list([]))).
%% []
%% '''
%%
%% Evaluates one element further in the iterator than the current
%% value.
%% @end
-spec join(Sep, iterator(Elem)) -> iterator(Sep | Elem).
join(Sep, #iterator{} = Iterator) ->
    unfold(fun ([]) ->
                   none;
               ([Elem | #iterator{} = FoldIterator]) ->
                   case next(FoldIterator) of
                       [] ->
                           {Elem, []};
                       [NextElem | #iterator{} = NextIterator] ->
                           {Elem, [Sep, NextElem | NextIterator]}
                    end;
               ([Elem | Tail]) ->
                   {Elem, Tail}
            end,
            next(Iterator)).

%% @doc
%% Takes a function `Fun' from `A's to `B's, and an `Iterator' of `A's and produces an
%% iterator of `B's by applying the function to every element in the iterator.
%% @end
-spec map(Fun :: fun((A) -> B), Iterator :: iterator(A)) -> iterator(B).
map(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | NextIterator] ->
                        [Fun(Elem) | map(Fun, NextIterator)];
                    [] ->
                        []
                end
        end).

%% @doc
%% Returns the sorted iterator formed by merging all the subiterators
%% of `IteratorOfIterators'. All subiterators must be sorted before
%% evaluating this function. When two elements compare equal, the
%% element from the subiterator with the lowest position in
%% `IteratorOfIterators' is picked before the other element.
%%
%% All iterators are fully evaluated during merge, infinite iterators
%% will never return.
%% @end
-spec merge(IteratorOfIterators :: iterator(iterator())) -> iterator().
merge(#iterator{} = IteratorOfIterators) ->
    ListOfIterators = to_list(IteratorOfIterators),
    Merged = lists:merge([to_list(I) || I <- ListOfIterators]),
    from_list(Merged).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted before
%% evaluating this function.  When two elements compare equal, the
%% element from `Iterator1' is picked before the element from
%% `Iterator2'.
%% @end
-spec merge(Iterator1 :: iterator(A), Iterator2 :: iterator(B)) ->
    iterator(A | B).
merge(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    Merged = lists:merge(to_list(Iterator1), to_list(Iterator2)),
    from_list(Merged).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted
%% according to the ordering function `Fun' before evaluating this
%% function. `Fun(A, B)' is to return `true' if `A' compares less than
%% or equal to `B' in the ordering, otherwise `false'. When two
%% elements compare equal, the element from `Iterator1' is picked
%% before the element from `Iterator2'.
%%
%% All iterators are fully evaluated during merge, infinite iterators
%% will never return.
%% @end
-spec merge(Fun :: fun((A, B) -> boolean()), Iterator1 :: iterator(A), Iterator2 :: iterator(B)) ->
    iterator(A | B).
merge(Fun, #iterator{} = Iterator1, #iterator{} = Iterator2) ->
    Merged = lists:merge(Fun, to_list(Iterator1), to_list(Iterator2)),
    from_list(Merged).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1',
%% `Iterator2', and `Iterator3'.  All of `Iterator1', `Iterator2', and
%% `Iterator3' must be sorted before evaluating this function. When
%% two elements compare equal, the element from `Iterator1', if there
%% is such an element, is picked before the other element, otherwise
%% the element from `Iterator2' is picked before the element from
%% `Iterator3'.
%%
%% All iterators are fully evaluated during merge, infinite iterators
%% will never return.
%% @end
-spec merge3(Iterator1 :: iterator(A),
             Iterator2 :: iterator(B),
             Iterator3 :: iterator(C)) -> iterator(A | B | C).
merge3(#iterator{} = Iterator1,
       #iterator{} = Iterator2,
       #iterator{} = Iterator3) ->
    Merged = lists:merge3(to_list(Iterator1),
                          to_list(Iterator2),
                          to_list(Iterator3)),
    from_list(Merged).

%% @doc
%% Returns the `N'th tail of `Iterator', that is, the subiterator of
%% `Iterator' starting at `N'+1 and continuing up to the end of the
%% iterator.
%% @end
-spec nthtail(N :: non_neg_integer(), iterator(Elem)) -> iterator(Elem).
nthtail(N, #iterator{} = Iterator) when is_integer(N), N >= 0 ->
    nthtail_loop(N, Iterator).

%% @doc
%% Returns an iterator with the elements in `Iterator' in reverse
%% order.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec reverse(Iterator :: iterator(A)) -> iterator(A).
reverse(#iterator{} = Iterator) ->
    list_wrap(fun lists:reverse/1, Iterator).

%% @doc
%% Returns a list with the elements in `Iterator' in reverse order,
%% with tail `Tail' appended.
%%
%% Example:
%% ```
%% > lists:reverse([1, 2, 3, 4], [a, b, c]).
%% [4,3,2,1,a,b,c]
%% '''
%%
%% The iterator `Iterator' will be fully evaluated, infinite iterators
%% will never return.
%% @end
-spec reverse(Iterator :: iterator(A),
              Tail :: iterator(B)) -> iterator(A | B).
reverse(#iterator{} = Iterator, #iterator{} = Tail) ->
    append(reverse(Iterator), Tail).

%% @see sublist/3
-spec sublist(Iterator :: iterator(Elem),
              Len :: non_neg_integer()) -> iterator(Elem).
sublist(#iterator{} = Iterator, Len) when is_integer(Len), Len >= 0->
    new(fun () ->
                case {Len, next(Iterator)} of
                    {0, _} ->
                        [];
                    {_, []} ->
                        [];
                    {Len, [Elem | NextIterator]} ->
                        [Elem | sublist(NextIterator, Len - 1)]
                end
        end).

%% @doc
%% Returns the portion of `Iterator' starting at `Start' and with
%% (maximum) `Len' elements. `Start' defaults to 1. It is not an error
%% for `Start+Len' to exceed the length of the iterator, in that case
%% the whole iterator is returned.
%% @end
-spec sublist(Iterator :: iterator(Elem),
              Start :: pos_integer(),
              Len :: non_neg_integer()) -> iterator(Elem).
sublist(#iterator{} = Iterator, Start, Len) when
      is_integer(Start),
      is_integer(Len),
      Start > 0,
      Len >= 0 ->
    sublist(nthtail(Start - 1, Iterator), Len).

%% @doc
%% Takes elements `Elem' from `Iterator' while `Pred(Elem)' returns
%% true, that is, the function returns the longest prefix of the
%% iterator for which all elements satisfy the predicate.
%% @end
-spec takewhile(Pred :: predicate(Elem), Iterator :: iterator(Elem)) ->
    iterator(Elem).
takewhile(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | NextIterator] ->
                        case Pred(Elem) of
                            true ->
                                [Elem | takewhile(Pred, NextIterator)];
                            false ->
                                []
                        end;
                    [] ->
                        []
                end
        end).

%% @doc
%% Partitions `Iterator' into two iterators, where the first iterator contains all
%% elements for which `Pred(Elem)' returns `true', and the second iterator
%% contains all elements for which `Pred(Elem)' returns `false'.
%%
%% Examples:
%% ```
%% > {Satisfying, NotSatisfying} = llists:partition(fun(A) -> A rem 2 == 1 end, llists:seq(1, 7)),
%% > {llists:to_list(Satisfying), llists:to_list(NotSatisfying)}.
%% {[1,3,5,7],[2,4,6]}
%% > {Satisfying, NotSatisfying} = llists:partition(fun(A) -> is_atom(A) end, llists:from_list([a,b,1,c,d,2,3,4,e])),
%% > {llists:to_list(Satisfying), llists:to_list(NotSatisfying)}.
%% {[a,b,c,d,e],[1,2,3,4]}
%% '''
%%
%% For a different way to partition a list, see splitwith/2.
%% @end
%% @see splitwith/2
-spec partition(Pred :: predicate(Elem), Iterator :: iterator(Elem)) ->
    {Satisfying :: iterator(Elem), NotSatisfying :: iterator(Elem)}.
partition(Pred, #iterator{} = Iterator) ->
    Satisfying = filter(Pred, Iterator),
    NotSatisfying = filter(fun (Elem) -> not Pred(Elem) end, Iterator),
    {Satisfying, NotSatisfying}.

%% @doc
%% Splits `Iterator1' into `Iterator2' and `Iterator3'. `Iterator2'
%% contains the first `N' elements and `Iterator3' the remaining
%% elements (the `N'th tail).
%%
%% Evaluates the first `N' elements of `Iterator1' to construct
%% `Iterator3'.
%% @end
-spec split(N :: non_neg_integer(), Iterator1 :: iterator(Elem)) ->
    {Iterator2 :: iterator(Elem), Iterator3 :: iterator(Elem)}.
split(N, #iterator{} = Iterator) when is_integer(N), N >= 0 ->
    {sublist(Iterator, N), nthtail(N, Iterator)}.

%% @doc
%% Partitions `Iterator' into two iterators according to `Pred'.
%% `splitwith/2' behaves as if it is defined as follows:
%%
%% ```
%% llists:splitwith(Pred, Iterator) ->
%%     {llists:takewhile(Pred, Iterator),
%%      llists:dropwhile(Pred, Iterator)}.
%% '''
%%
%% Examples:
%% ```
%% > {Before, After} = llists:splitwith(fun(A) -> A rem 2 == 1 end, llists:seq(1, 7)),
%% > {llists:to_list(Before), llists:to_list(After)}.
%% {[1],[2,3,4,5,6,7]}
%% > {Before, After} = lists:splitwith(fun(A) -> is_atom(A) end, [a,b,1,c,d,2,3,4,e]),
%% > {llists:to_list(Before), llists:to_list(After)}.
%% {[a,b],[1,c,d,2,3,4,e]}
%% '''
%%
%% For a different way to partition an iterator, see partition/2.
%%
%% Evaluates the elements of `Iterator' for which `Pred(Elem)' returns
%% `false'. If `Pred' never returns `false', infinite iterators will
%% not return.
%% @end
%% @see partition/2
-spec splitwith(Pred :: predicate(Elem), Iterator :: iterator(Elem)) ->
    {Iterator1 :: iterator(Elem), Iterator2 :: iterator(Elem)}.
splitwith(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    {takewhile(Pred, Iterator), dropwhile(Pred, Iterator)}.

%% @doc
%% Returns a new iterator `Iterator3' that is a copy of `Iterator1',
%% subjected to the following procedure: for each element in
%% `Iterator2', its first occurrence in `Iterator1' is deleted.
%%
%% Example:
%% ```
%% > lists:subtract("123212", "212").
%% "312".
%% '''
%%
%% `Iterator2' is fully evaluated, infinite iterators will never return.
%% @end
-spec subtract(Iterator1 :: iterator(Elem), Iterator2 :: iterator()) ->
    iterator(Elem).
subtract(#iterator{} = Base, #iterator{} = RemoveIterator) ->
    unfold(fun Next({[], _Remove}) ->
                   none;
               Next({[Elem | #iterator{} = BaseIterator], Remove}) ->
                   case drop(Elem, Remove) of
                       none ->
                           {Elem, {next(BaseIterator), Remove}};
                       {dropped, NewRemove} ->
                           Next({next(BaseIterator), NewRemove})
                   end
           end,
           {next(Base), to_list(RemoveIterator)}).

%% @doc
%% Returns the sorted iterator formed by merging all the subiterators of
%% `IteratorOfIterators'. All subiterators must be sorted and contain no duplicates
%% before evaluating this function. When two elements compare equal,
%% the element from the subiterator with the lowest position in
%% `IteratorOfIterators' is picked and the other is deleted.
%%
%% All iterators are fully evaluated, infinite iterators will never
%% return.
%% @end
-spec umerge(IteratorOfIterators :: iterator(iterator())) -> iterator().
umerge(#iterator{} = IteratorOfIterators) ->
    ListOfIterators = to_list(IteratorOfIterators),
    Merged = lists:umerge([to_list(I) || I <- ListOfIterators]),
    from_list(Merged).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted and
%% contain no duplicates before evaluating this function. When two
%% elements compare equal, the element from `Iterator1' is picked and
%% the one from `Iterator2' is deleted.
%%
%% Both iterators are fully evaluated, infinite iterators will never
%% return.
%% @end
-spec umerge(Iterator1 :: iterator(A), Iterator2 :: iterator(B)) ->
    iterator(A | B).
umerge(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    Merged = lists:umerge(to_list(Iterator1), to_list(Iterator2)),
    from_list(Merged).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted
%% according to the ordering function `Fun' and contain no duplicates
%% before evaluating this function. `Fun(A, B)' is to return `true' if
%% `A' compares less than or equal to `B' in the ordering, otherwise
%% `false'. When two elements compare equal, the element from
%% `Iterator1' is picked and the one from `Iterator2' is deleted.
%%
%% Both iterators are fully evaluated, infinite iterators will never
%% return.
%% @end
-spec umerge(Fun :: fun((A, B) -> boolean()), Iterator1 :: iterator(A), Iterator2 :: iterator(B)) ->
    iterator(A | B).
umerge(Fun, #iterator{} = Iterator1, #iterator{} = Iterator2) ->
    Merged = lists:umerge(Fun, to_list(Iterator1), to_list(Iterator2)),
    from_list(Merged).

%%%===================================================================
%%% API - Iterator Evaluation
%%%===================================================================

-spec next(iterator(Elem)) -> lazy_list(Elem).
next(#iterator{next=Next}) ->
    Next().

-spec to_list(iterator(Elem)) -> [Elem].
to_list(#iterator{} = Iterator) ->
    to_list_loop(next(Iterator)).

%% @doc
%% Returns the head of `Iterator', that is, the first element, for
%% example:
%%
%% ```
%% > llists:hd(llists:seq(1, 5)).
%% 1
%% '''
%%
%% Failure: `badarg' if `Iterator' is empty.
%% @end
-spec hd(Iterator :: iterator(Elem)) -> Elem.
hd(#iterator{} = Iterator) ->
    erlang:hd(next(Iterator)).

%% @doc
%% Returns the length of `Iterator', for example:
%%
%% ```
%% > llists:length(llists:seq(1, 9)).
%% 9
%% '''
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
length(#iterator{} = Iterator) ->
    foldl(fun (_, Acc) -> Acc + 1 end, 0, Iterator).

%% @doc
%% Returns the tail of `Iterator', that is, the iterator minus the
%% first element, for example:
%%
%% ```
%% > llists:to_list(
%% >  llists:tl(
%% >   llists:from_list([geesties, guilies, beasties]))).
%% [guilies, beasties]
%% '''
%%
%% Failure: `badarg' if `Iterator' is empty.
%% @end
-spec tl(Iterator :: iterator(Elem)) -> iterator(Elem).
tl(#iterator{} = Iterator) ->
    erlang:tl(next(Iterator)).

%% @doc
%% Returns `true' if `Pred(Elem)' returns `true' for all elements
%% `Elem' in `Iterator'.
%% 
%% Stops evaluating `Iterator' when `Pred(Elem)' returns `false' or
%% when `Iterator' is empty.
%% @end
-spec all(Pred :: predicate(Elem), Iterator :: iterator(Elem)) -> boolean().
all(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    all_loop(Pred, next(Iterator)).

%% @doc
%% Returns `true' if `Pred(Elem)' returns `true' for at least one
%% element `Elem' in `Iterator'. 
%%
%% Stops evaluating `Iterator' when `Pred(Elem)' returns `true' or
%% when `Iterator' is empty.
%% @end
-spec any(predicate(Elem), iterator(Elem)) -> boolean().
any(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    any_loop(Pred, next(Iterator)).

%% @doc
%% Calls `Fun(Elem, AccIn)' on successive elements `A' of `Iterator',
%% starting with `AccIn' == `Acc0'. `Fun/2' must return a new
%% accumulator, which is passed to the next call. The function returns
%% the final value of the accumulator. `Acc0' is returned if the
%% iterator is empty.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec foldl(fold(A, AccIn :: any(), AccOut), Acc0 :: any(), iterator(A)) -> AccOut.
foldl(Fun, Acc0, #iterator{} = Iterator) when is_function(Fun, 2) ->
    foldl_loop(Fun, Acc0, next(Iterator)).

%% @doc
%% Like `foldl/3', but the list is traversed from right to left.
%%
%% Example:
%% ```
%% > P = fun(A, AccIn) -> io:format("~p ", [A]), AccIn end.
%% #Fun<erl_eval.12.2225172>
%% > llists:foldl(P, void, llists:seq(1, 3)).
%% 1 2 3 void
%% > lists:foldr(P, void, llists:seq(1, 3)).
%% 3 2 1 void
%% '''
%%
%% The iterator is fully evaluated before the fold begins, infinite
%% iterators will never return. `foldl/3' does not fully evaluate the
%% iterator and is usually preferred to `foldr/3'.
%% @end
%% @see foldl/3
-spec foldr(fold(A, AccIn :: any(), AccOut), Acc0 :: any(), Iterator :: iterator(A)) -> AccOut.
foldr(Fun, Acc0, #iterator{} = Iterator) when is_function(Fun, 2) ->
    foldl(Fun, Acc0, reverse(Iterator)).

%% @doc
%% Calls `Fun(Elem)' for each element `Elem' in `Iterator'. This
%% function is used for its side effects and the evaluation order is
%% defined to be the same as the order of the elements in the
%% iterator.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec foreach(Fun :: fun((Elem) -> any()), Iterator :: iterator(Elem)) -> ok.
foreach(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    foreach_loop(Fun, next(Iterator)).

%% @doc
%% Returns the last element in `Iterator'.
%% 
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec last(iterator(Elem)) -> Elem.
last(#iterator{} = Iterator) ->
    Last = foldl(fun (Elem, _Acc) -> {last, Elem} end,
                 undefined,
                 Iterator),
    case Last of
        undefined ->
            % Because that's how lists:last([]) crashes.
            error(function_clause);
        {last, Elem} ->
            Elem
    end.

%% @doc
%% Combines the operations of `map/2' and `foldl/3' into one pass.
%%
%% Example:
%% Summing the elements in an iterator and double them at the same time:
%% ```
%% > {Mapped, Acc} = llists:mapfoldl(fun(X, Sum) -> {2*X, X+Sum} end, 0, llists:seq(1,5)),
%% > {llists:to_list(Mapped), Acc}.
%% {[2,4,6,8,10],15}
%% '''
%%
%% The iterator is fully evaluated before the mapfold begins, infinite
%% iterators will never return.
%% @end
-spec mapfoldl(Fun :: fun((A, AccIn :: Acc0 | AccOut) -> {B, AccOut}), Acc0, Iterator1 :: iterator(A)) ->
    {Iterator2 :: iterator(B), Acc1 :: AccOut}.
mapfoldl(Fun, Acc0, #iterator{} = Iterator) ->
    {Mapped, AccOut} = lists:mapfoldl(Fun, Acc0, to_list(Iterator)),
    {llists:from_list(Mapped), AccOut}.

%% @doc
%% Combines the operations of map/2 and foldr/3 into one pass.
%%
%% The iterator is fully evaluated before the mapfold begins, infinite
%% iterators will never return.
%% @end
-spec mapfoldr(Fun :: fun((A, AccIn :: Acc0 | AccOut) -> {B, AccOut}), Acc0, Iterator1 :: iterator(A)) ->
    {Iterator2 :: iterator(B), Acc1 :: AccOut}.
mapfoldr(Fun, Acc0, #iterator{} = Iterator) ->
    {Mapped, AccOut} = lists:mapfoldr(Fun, Acc0, to_list(Iterator)),
    {llists:from_list(Mapped), AccOut}.

%% @doc
%% Returns the first element of `Iterator' that compares greater than
%% or equal to all other elements of `Iterator'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec max(Iterator :: iterator(Elem)) -> Elem.
max(#iterator{} = Iterator) ->
    Max = foldl(fun (Elem, undefined) ->
                        {max, Elem};
                    (Elem, {max, Max}) when Elem > Max ->
                        {max, Elem};
                    (_Elem, Acc) ->
                        Acc
                end,
                undefined,
                Iterator),
    case Max of
        undefined ->
            % Because that's how lists:max([]) crashes.
            error(function_clause);
        {max, Elem} ->
            Elem
    end.

%% @doc
%% Returns `true' if `Elem' matches some element of `Iterator',
%% otherwise `false'.
%%
%% Stops evaluating `Iterator' when a match is found or when
%% `Iterator' is empty.
%% @end
-spec member(Elem :: any(), Iterator :: iterator()) -> boolean().
member(Elem, #iterator{} = Iterator) ->
    any(fun (E) when E =:= Elem -> true;
            (_) -> false
        end,
        Iterator).

%% @doc
%% Returns the first element of `Iterator' that compares less than or
%% equal to all other elements of `Iterator'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec min(Iterator :: iterator(Elem)) -> Elem.
min(#iterator{} = Iterator) ->
    Min = foldl(fun (Elem, undefined) ->
                        {min, Elem};
                    (Elem, {min, Min}) when Elem < Min ->
                        {min, Elem};
                    (_Elem, Acc) ->
                        Acc
                end,
                undefined,
                Iterator),
    case Min of
        undefined ->
            % Because that's how lists:min([]) crashes.
            error(function_clause);
        {min, Elem} ->
            Elem
    end.

%% @doc
%% Returns the `N'th element of `Iterator'.
%%
%% Example:
%% ```
%% > lists:nth(3, [a, b, c, d, e]).
%% c
%% '''
%% @end
-spec nth(N :: pos_integer(), Iterator :: iterator(Elem)) -> Elem.
nth(N, #iterator{} = Iterator) when N > 0 ->
    Tail = nthtail(N - 1, Iterator),
    case next(Tail) of
        [] ->
            % Because that's how lists:nth/2 & llists:nthtail/2 crash.
            error(function_clause);
        [Elem | #iterator{}] ->
            Elem
    end.

%% @doc
%% Returns `true' if `Iterator1' is a prefix of `Iterator2', otherwise `false'.
%%
%% Both iterators will be evaluated until the point they diverge. If
%% both iterators are identical and infinite, will never return.
%% @end
-spec prefix(Iterator1 :: iterator(), Iterator2 :: iterator()) -> boolean().
prefix(#iterator{} = Prefix, #iterator{} = Iterator) ->
    prefix_loop(next(Prefix), next(Iterator)).

%% @doc
%% Returns an iterator containing the sorted elements of `Iterator1'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec sort(Iterator1 :: iterator(Elem)) -> Iterator2 :: iterator(Elem).
sort(#iterator{} = Iterator) ->
    list_wrap(fun lists:sort/1, Iterator).

%% @doc
%% Returns an iterator containing the sorted elements of `Iterator1',
%% according to the ordering function `Fun'. `Fun(A, B)' is to return
%% `true' if `A' compares less than or equal to `B' in the ordering,
%% otherwise `false'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec sort(Fun :: fun((A, A) -> boolean()), Iterator1 :: iterator(A)) ->
    Iterator2 :: iterator(A).
sort(Fun, #iterator{} = Iterator) when is_function(Fun, 2) ->
    list_wrap(fun (I) -> lists:sort(Fun, I) end, Iterator).

%% @doc
%% If there is a `Value' in `Iterator' such that `Pred(Value)' returns `true',
%% returns `{value, Value}' for the first such `Value', otherwise returns
%% `false'.
%%
%% The iterator is evaluated until a match is found. If no match is
%% ever found, infinite iterators will never return.
%% @end
-spec search(Pred :: predicate(Value), Iterator :: iterator()) ->
    {value, Value} | false.
search(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    search_loop(Pred, next(Iterator)).

%% @doc
%% Returns `true' if `Iterator1' is a suffix of `Iterator2', otherwise
%% `false'.
%%
%% Both `Iterator1' and `Iterator2' are fully evaluated, infinite
%% iterators will never return.
%% @end
-spec suffix(Iterator1 :: iterator(), Iterator2 :: iterator()) -> boolean().
suffix(#iterator{} = Suffix, #iterator{} = Iterator) ->
    prefix(reverse(Suffix), reverse(Iterator)).

%% @doc
%% Returns the sum of the elements in `Iterator'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec sum(Iterator :: iterator(number())) -> number().
sum(#iterator{} = Iterator) ->
    foldl(fun (Elem, Acc) -> Elem + Acc end, 0, Iterator).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec new(fun(() -> lazy_list(Elem))) -> iterator(Elem).
new(Next) ->
    #iterator{next=Next}.

list_wrap(ListFun, Iterator) ->
    llists:from_list(ListFun(llists:to_list(Iterator))).

drop(Elem, List) ->
    case lists:member(Elem, List) of
        true ->
            {dropped, List -- [Elem]};
        false ->
            none
    end.

nthtail_loop(0, #iterator{} = Iterator) ->
    Iterator;
nthtail_loop(N, #iterator{} = Iterator) ->
    nthtail_loop(N - 1, next(Iterator));
nthtail_loop(N, [_Head | #iterator{} = Iterator]) ->
    nthtail_loop(N, Iterator).

to_list_loop([]) ->
    [];
to_list_loop([Head | #iterator{} = Iterator]) ->
    [Head | to_list_loop(next(Iterator))].

all_loop(_Pred, []) ->
    true;
all_loop(Pred, [Head | #iterator{} = Iterator]) ->
    case Pred(Head) of
        true ->
            all_loop(Pred, next(Iterator));
        false ->
            false
    end.

any_loop(_Pred, []) ->
    false;
any_loop(Pred, [Head | #iterator{} = Iterator]) ->
    case Pred(Head) of
        true ->
            true;
        false ->
            any_loop(Pred, next(Iterator))
    end.

foldl_loop(_Fun, Acc, []) ->
    Acc;
foldl_loop(Fun, Acc, [Elem | #iterator{} = Iterator]) ->
    foldl_loop(Fun, Fun(Elem, Acc), next(Iterator)).

foreach_loop(_Fun, []) ->
    ok;
foreach_loop(Fun, [Elem | #iterator{} = Iterator]) ->
    _ = Fun(Elem),
    foreach_loop(Fun, next(Iterator)).

prefix_loop([], _) ->
    true;
prefix_loop(_, []) ->
    % If first iterator is longer.
    false;
prefix_loop([Elem | #iterator{} = Iterator1], [Elem | #iterator{} = Iterator2]) ->
    prefix_loop(next(Iterator1), next(Iterator2));
prefix_loop([_Elem1 | #iterator{}], [_Elem2 | #iterator{}]) ->
    % If elements differ.
    false.

search_loop(_Pred, []) ->
    false;
search_loop(Pred, [Value | #iterator{} = Iterator]) ->
    case Pred(Value) of
        true ->
            {value, Value};
        false ->
            search_loop(Pred, next(Iterator))
    end.
