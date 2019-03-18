%%%-------------------------------------------------------------------
%%% @doc
%%% A lazily evaluated lists module. This module provides an iterator
%%% type which is an opaque record wrapped around a list continuation.
%%% These iterators are then used to provide a version of the stdlib
%%% `lists' functions which only evaluate elements of the iterator
%%% when demanded.
%%%
%%% Several simple iterator constructors are provided as well as a
%%% general purpose `unfold/2' constructor.
%%%
%%% The interface for this module attempts to follow the `lists'
%%% behaviour as closely as possible. Guidelines for how past and
%%% future translation is performed is as follows:
%%%
%%% <ul>
%%% <li>Any input lists are changed to expect iterators.</li>
%%% <li>Any output lists are changed to be iterators.</li>
%%% <li>Any numeric counts for repetition are changed to allow
%%% 'infinity' as values and to be able to return infinite
%%% iterators.</li>
%%% <li>On error, the same exception should be raised, though it may
%%% not be raised until the triggering element of an iterator is
%%% evaluated.</li>
%%% <li>Iteration evaluation behaviour is documented.</li>
%%% </ul>
%%%
%%% As few functions outside of `lists' have been implemented as
%%% possible, in order to have the best chance of keeping the
%%% namespace clean for future additions to the `lists' module. New
%%% functionality is instead implemented in the `llists_utils' module.
%%% @end
%%%-------------------------------------------------------------------
-module(llists).

-include("include/llists.hrl").

-type iterator() :: iterator(any()).
-opaque iterator(Over) :: #iterator{next :: fun(() -> lazy_list(Over))}.
-type tuple_iterator() :: iterator(tuple()).
-type lazy_list(Over) :: nonempty_improper_list(Over, iterator(Over)) | [].

-type accumulator() :: any().
-type combine(A, B, Out) :: fun((A, B) -> Out).
-type combine3(A, B, C, Out) :: fun((A, B, C) -> Out).
-type compare(A, B) :: fun((A, B) -> boolean()).
-type filtermap(A, B) :: fun((A) -> boolean() | {true, B}).
-type fold(Elem, AccIn, AccOut) :: fun((Elem, AccIn) -> AccOut).
-type map(A, B) :: fun((A) -> B).
-type mapfold(A, AccIn, B, AccOut) :: fun((A, AccIn) -> {B, AccOut}).
-type predicate(Elem) :: fun((Elem) -> boolean()).
-type unfold(Elem, AccIn, AccOut) :: fun((AccIn) -> {Elem, AccOut} | none).

%% API
-export([% Iterator construction.
         is_iterator/1,
         from_list/1,
         from_map/1,
         unfold/2,
         duplicate/2,
         seq/2,
         seq/3,
         % Iterator utilities.
         next/1,
         hd/1,
         tl/1,
         append/1,
         append/2,
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
         keydelete/3,
         keymap/3,
         keymerge/3,
         keyreplace/4,
         keysort/2,
         keystore/4,
         keytake/3,
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
         sort/1,
         sort/2,
         split/2,
         splitwith/2,
         subtract/2,
         ukeymerge/3,
         ukeysort/2,
         umerge/1,
         umerge/2,
         umerge/3,
         umerge3/3,
         unzip/1,
         unzip3/1,
         usort/1,
         usort/2,
         zip/2,
         zip3/3,
         zipwith/3,
         zipwith3/4,
         % Iterator evaluation.
         to_list/1,
         to_map/1,
         length/1,
         all/2,
         any/2,
         concat/1,
         foldl/3,
         foldr/3,
         foreach/2,
         keyfind/3,
         keymember/3,
         keysearch/3,
         last/1,
         mapfoldl/3,
         mapfoldr/3,
         max/1,
         member/2,
         min/1,
         nth/2,
         prefix/2,
         search/2,
         suffix/2,
         sum/1]).

-export_type([iterator/1,
              lazy_list/1,
              combine/3,
              combine3/4,
              compare/2,
              filtermap/2,
              fold/3,
              map/2,
              mapfold/4,
              predicate/1,
              unfold/3]).

-compile({no_auto_import,
          [hd/1,
           length/1,
           max/1,
           min/1,
           tl/1]}).

%%%===================================================================
%%% API - Iterator Construction
%%%===================================================================

%% @doc
%% Tests if the given `Candidate' is an iterator, returns `true' if it
%% and `false' otherwise.
%% @end
-spec is_iterator(Candidate) -> boolean() when
      Candidate :: any().
is_iterator(#iterator{}) ->
    true;
is_iterator(_) ->
    false.

%% @doc
%% Construct a new iterator from an existing list. Each element of the
%% list will be returned in order by the returned iterator.
%% @end
-spec from_list(List) -> Iterator when
      List :: list(Elem),
      Iterator :: iterator(Elem).
from_list(List) when is_list(List) ->
    unfold(fun ([]) -> none;
               ([Head | Tail]) -> {Head, Tail}
           end,
           List).

%% @doc
%% Construct a new iterator from an existing map. Each `{Key, Value}'
%% tuple of the map will be returned in an arbitrary order by the
%% returned iterator.
%% @end
-spec from_map(Map) -> Iterator when
      Map :: maps:map(Key, Value),
      Iterator :: iterator({Key, Value}).
from_map(Map) when is_map(Map) ->
    unfold(fun (MapIterator) ->
                   case maps:next(MapIterator) of
                       none ->
                           none;
                       {Key, Value, NextIterator} ->
                           {{Key, Value}, NextIterator}
                   end
           end,
           maps:iterator(Map)).

%% @doc
%% Construct a new iterator from a `Fun(AccIn)' function and an
%% initial accumulator value `Acc0'. When an element is demanded of
%% the iterator, `Fun' will be invoked with the current accumulator to
%% produce a value. `Fun' is expected to return a tuple of
%% `{Elem, AccOut}': the element to produce and the new accumulator
%% value. If iteration is complete, `Fun' should return `none'.
%% @end
-spec unfold(Fun, Acc0) -> Iterator when
      Fun :: unfold(Elem, AccIn :: Acc0 | AccOut, AccOut),
      Acc0 :: accumulator(),
      Iterator :: iterator(Elem).
unfold(Fun, Acc) when is_function(Fun, 1) ->
    new(fun () ->
                case Fun(Acc) of
                    {Elem, NewAcc} ->
                        [Elem | unfold(Fun, NewAcc)];
                    none ->
                        []
                end
        end).

%% @doc
%% Returns an iterator containing `N' copies of term `Elem'. If `N' is
%% `infinity' iterator will return infinite copies of `Elem'.
%% @end
-spec duplicate(N , Elem) -> Iterator when
      N :: infinity | non_neg_integer(),
      Elem :: any(),
      Iterator :: iterator(Elem).
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
-spec seq(From, To) -> Iterator when
      From :: integer(),
      To :: integer(),
      Iterator :: iterator(integer()).
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
-spec seq(From, To, Incr) -> Iterator when
      From :: integer(),
      To :: infinity | '-infinity' | integer(),
      Incr :: integer(),
      Iterator :: iterator(integer()).
seq(From, infinity, Incr) when
      is_integer(From),
      is_integer(Incr),
      Incr > 0 ->
    unfold(fun (Acc) -> {Acc, Acc + Incr} end, From);
seq(From, '-infinity', Incr) when
      is_integer(From),
      is_integer(Incr),
      Incr < 0 ->
    unfold(fun (Acc) -> {Acc, Acc + Incr} end, From);
seq(From, To, Incr) when
      is_integer(From),
      is_integer(To),
      is_integer(Incr),
      Incr > 0,
      From - Incr =< To ->
    unfold(fun (Acc) when Acc > To -> none;
               (Acc) -> {Acc, Acc + Incr}
           end,
           From);
seq(From, To, Incr) when
      is_integer(From),
      is_integer(To),
      is_integer(Incr),
      Incr < 0,
      From - Incr >= To ->
    unfold(fun (Acc) when Acc < To -> none;
               (Acc) -> {Acc, Acc + Incr}
           end,
           From);
seq(From, From, 0) when
      is_integer(From) ->
    from_list([From]).

%%%===================================================================
%%% API - Iterator Utilities
%%%===================================================================

%% @doc
%% Demand an element from `Iterator'. Will return either an improper
%% list containing the next element and an iterator as a continuation,
%% or an empty list if iteration is complete.
%%
%% Examples:
%%
%% ```
%% > llists:next(llists:seq(1, 5)).
%% [1|{iterator,#Fun<llists.1.134155648>}]
%% > llists:next(llists:from_list([])).
%% []
%% '''
%% @end
-spec next(Iterator) -> LazyList when
      Iterator :: iterator(Elem),
      LazyList :: lazy_list(Elem).
next(#iterator{next=Next}) ->
    Next().

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
-spec hd(Iterator) -> Elem when
      Iterator :: iterator(Elem).
hd(#iterator{} = Iterator) ->
    erlang:hd(next(Iterator)).

%% @doc
%% Returns the tail of `Iterator1', that is, the iterator minus the
%% first element, for example:
%%
%% ```
%% > llists:to_list(
%% >  llists:tl(
%% >   llists:from_list([geesties, guilies, beasties]))).
%% [guilies, beasties]
%% '''
%%
%% Failure: `badarg' if `Iterator1' is empty.
%% @end
-spec tl(Iterator1) -> Iterator2 when
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
tl(#iterator{} = Iterator) ->
    erlang:tl(next(Iterator)).

%% @doc
%% Returns an iterator in which all the subiterators of
%% `IteratorOfIterators' have been appended.
%% @end
-spec append(IteratorOfIterators) -> Iterator when
      IteratorOfIterators :: iterator(iterator()),
      Iterator :: iterator().
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
-spec append(Iterator1, Iterator2) -> Iterator3 when
      Iterator1 :: iterator(Elem1),
      Iterator2 :: iterator(Elem2),
      Iterator3 :: iterator(Elem1 | Elem2).
append(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    append(from_list([Iterator1, Iterator2])).

%% @doc
%% Returns a copy of `Iterator1' where the first element matching
%% `Elem' is deleted, if there is such an element.
%% @end
-spec delete(Elem1, Iterator1) -> Iterator2 when
      Elem1 :: any(),
      Iterator1 :: iterator(Elem2),
      Iterator2 :: iterator(Elem2),
      Elem2 :: any().
delete(Elem, #iterator{} = Iterator) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] ->
                        next(NextIterator);
                    [Other | #iterator{} = NextIterator] ->
                        [Other | delete(Elem, NextIterator)];
                    [] ->
                        []
                end
        end).

%% @doc
%% Drops the last element of a `Iterator1'. The `Iterator1' is to be
%% non-empty, otherwise the function crashes with a `function_clause'.
%%
%% Evaluates one element further in the iterator than the current
%% value.
%% @end
-spec droplast(Iterator1) -> Iterator2 when
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
droplast(#iterator{} = Iterator) ->
    % Notice the missing case for [] here, which forces the expected
    % error on an empty list.
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
%% Drops elements `Elem' from `Iterator1' while `Pred(Elem)' returns
%% true and returns the remaining iterator.
%% @end
-spec dropwhile(Pred, Iterator1) -> Iterator2 when
      Pred :: predicate(Elem),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
dropwhile(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] ->
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
%% `Filtered' is an iterator of all elements `Elem' in `Iterator1' for
%% which `Pred(Elem)' returns `true'.
%% @end
-spec filter(Pred, Iterator1) -> Iterator2 when
      Pred :: predicate(Elem),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
filter(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] ->
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
%% Calls `Fun(Elem)' on successive elements `Elem' of `Iterator1'.
%% `Fun/1' must return either a Boolean or a tuple `{true, Value}'.
%% The function returns the iterator of elements for which `Fun'
%% returns a new value, where a value of `true' is synonymous with
%% `{true, Elem}'.
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
%% ```
%% > llists:to_list(
%% >  llists:filtermap(
%% >   fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end,
%% >   llists:seq(1, 5))).
%% [1,2]
%% '''
%% @end
-spec filtermap(Fun, Iterator1) -> Iterator2 when
      Fun :: filtermap(Elem, Value),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem | Value).
filtermap(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] ->
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
-spec flatlength(DeepIterator) -> Length when
      DeepIterator :: iterator(any() | iterator()),
      Length :: non_neg_integer().
flatlength(#iterator{} = DeepIterator) ->
    length(flatten(DeepIterator)).

%% @doc
%% Takes a function from `A's to iterators of `B's, and an iterator of
%% `A's (`Iterator1') and produces an iterator of `B's (`Iterator2')
%% by applying the function to every element in `Iterator1' and
%% appending the resulting iterators.
%%
%% That is, flatmap behaves as if it had been defined as follows:
%%
%% ```
%% llists:flatmap(Fun, Iterator) ->
%%     llists:append(llists:map(Fun, Iterator)).
%% '''
%%
%% Example:
%% ```
%% > llists:to_list(
%% >  llists:flatmap(
%% >   fun(X)->llists:from_list([X,X]) end,
%% >   llists:from_list([a,b,c]))).
%% [a,a,b,b,c,c]
%% '''
-spec flatmap(Fun, Iterator1) -> Iterator2 when
      Fun :: map(A, iterator(B)),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B).
flatmap(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    llists:append(llists:map(Fun, Iterator)).

%% @doc
%% Returns a flattened version of `DeepIterator'.
%% @end
-spec flatten(DeepIterator) -> Iterator when
      DeepIterator :: iterator(any() | iterator()),
      Iterator :: iterator().
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
-spec flatten(DeepIterator, TailIterator) -> Iterator when
      DeepIterator :: iterator(any() | iterator()),
      TailIterator :: iterator(),
      Iterator :: iterator().
flatten(#iterator{} = DeepIterator, #iterator{} = Tail) ->
    append(flatten(DeepIterator), Tail).

%% @doc
%% Inserts `Sep' between each element in `Iterator1'. Has no effect on
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
-spec join(Sep, Iterator1) -> Iterator2 when
      Sep :: any(),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Sep | Elem).
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
%% Returns a copy of `TupleIterator1' where the first occurrence of a tuple
%% whose `N'th element compares equal to `Key' is deleted, if there is
%% such a tuple.
%% @end
-spec keydelete(Key, N, TupleIterator1) -> TupleIterator2 when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem),
      TupleIterator2 :: iterator(Elem).
keydelete(Key, N, #iterator{} = Iterator) when N > 0 ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] when Key == element(N, Elem) ->
                        next(NextIterator);
                    [Elem | #iterator{} = NextIterator] ->
                        [Elem | keydelete(Key, N, NextIterator)];
                    [] ->
                        []
                end
        end).

%% @doc
%% Returns an iterator of tuples where, for each tuple in
%% `TupleIterator1', the `N'th element `Term1' of the tuple has been
%% replaced with the result of calling `Fun(Term1)'.
%%
%% Examples:
%% ```
%% > Fun = fun(Atom) -> atom_to_list(Atom) end.
%% #Fun<erl_eval.6.10732646>
%% 2> llists:to_list(
%% 2>  llists:keymap(
%% 2>   Fun,
%% 2>   2,
%% 2>   llists:from_list([{name,jane,22},{name,lizzie,20},{name,lydia,15}]))).
%% [{name,"jane",22},{name,"lizzie",20},{name,"lydia",15}]
%% '''
%% @end
-spec keymap(Fun, N, TupleIterator1) -> TupleIterator2 when
      Fun :: map(Term1 :: any(), Term2 :: any()),
      N :: pos_integer,
      TupleIterator1 :: tuple_iterator(),
      TupleIterator2 :: tuple_iterator().
keymap(Fun, N, #iterator{} = Iterator) when is_function(Fun, 1), N > 0 ->
    map(fun (Tuple) ->
                Modified = Fun(element(N, Tuple)),
                setelement(N, Tuple, Modified)
        end,
        Iterator).

%% @doc
%% Returns the sorted iterator formed by merging `TupleIterator1' and
%% `TupleIterator2'. The merge is performed on the `N'th element of
%% each tuple. Both `TupleIterator1' and `TupleIterator2' must be
%% key-sorted before evaluating this function. When two tuples compare
%% equal, the tuple from `TupleIterator1' is picked before the tuple
%% from `TupleIterator2'.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec keymerge(N, TupleIterator1, TupleIterator2) -> TupleIterator3 when
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem1),
      TupleIterator2 :: iterator(Elem2),
      TupleIterator3 :: iterator(Elem1 | Elem2),
      Elem1 :: tuple(),
      Elem2 :: tuple().
keymerge(N, #iterator{} = Iterator1, #iterator{} = Iterator2) when N > 0 ->
    Compare = fun (A, B) -> element(N, A) =< element(N, B) end,
    fmerge(Compare, [Iterator1, Iterator2]).

%% @doc
%% Returns a copy of `TupleIterator1' where the first occurrence of a T
%% tuple whose `N'th element compares equal to `Key' is replaced with
%% `NewTuple', if there is such a tuple `T'.
%% @end
-spec keyreplace(Key, N, TupleIterator1, NewTuple) -> TupleIterator2 when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem),
      NewTuple :: tuple(),
      TupleIterator2 :: iterator(Elem | NewTuple).
keyreplace(Key, N, #iterator{} = Iterator, NewTuple) when N > 0, is_tuple(NewTuple) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] when Key == element(N, Elem) ->
                        [NewTuple | NextIterator];
                    [Elem | #iterator{} = NextIterator] ->
                        [Elem | keyreplace(Key, N, NextIterator, NewTuple)];
                    [] ->
                        []
                end
        end).

%% @doc
%% Returns an iterator containing the sorted elements of iterator
%% `TupleIterator1'. Sorting is performed on the `N'th element of the
%% tuples. The sort is stable.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec keysort(N, TupleIterator1) -> TupleIterator2 when
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem),
      TupleIterator2 :: iterator(Elem),
      Elem :: tuple().
keysort(N, #iterator{} = Iterator) when N > 0 ->
    list_wrap(fun (I) -> lists:keysort(N, I) end, Iterator).

%% @doc
%% Returns a copy of `TupleIterator1' where the first occurrence of a
%% tuple `T' whose `N'th element compares equal to `Key' is replaced
%% with `NewTuple', if there is such a tuple `T'. If there is no such
%% tuple `T', a copy of `TupleIterator1' where `NewTuple' has been
%% appended to the end is returned.
%% @end
-spec keystore(Key, N, TupleIterator1, NewTuple) -> TupleIterator2 when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem),
      NewTuple :: tuple(),
      TupleIterator2 :: iterator(Elem | NewTuple).
keystore(Key, N, #iterator{} = Iterator, NewTuple) when
      N > 0, is_tuple(NewTuple) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] when Key == element(N, Elem) ->
                        [NewTuple | NextIterator];
                    [Elem | #iterator{} = NextIterator] ->
                        [Elem | keystore(Key, N, NextIterator, NewTuple)];
                    [] ->
                        [NewTuple | from_list([])]
                end
        end).

%% @doc
%% Searches the iterator of tuples `TupleIterator1' for a tuple whose
%% `N'th element compares equal to `Key'. Returns
%% `{value, Tuple, TupleIterator2}' if such a tuple is found,
%% otherwise `false'.  `TupleIterator2' is a copy of `TupleIterator1'
%% where the first occurrence of `Tuple' has been removed.
%%
%% Evaluates `TupleIterator1' until a match is found. Iterating over
%% `TupleIterator2' will evaluate the same elements again. If no match
%% is found, infinite iterators will never return.
%% @end
-spec keytake(Key, N, TupleIterator1) -> {value, Tuple, TupleIterator2} when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem),
      Tuple :: tuple(),
      TupleIterator2 :: iterator(Elem).
keytake(Key, N, #iterator{} = Iterator) when N > 0 ->
    case keysearch(Key, N, Iterator) of
        {value, Tuple} ->
            {value, Tuple, keydelete(Key, N, Iterator)};
        false ->
            false
    end.

%% @doc
%% Takes a function `Fun' from `A's to `B's, and an `Iterator1' of
%% `A's and produces an `Iterator2' of `B's by applying the function
%% to every element in the iterator.
%% @end
-spec map(Fun, Iterator1) -> Iterator2 when
      Fun :: map(A, B),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B).
map(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] ->
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
%% The first element of each subiterator will be evaluated.
%% @end
-spec merge(IteratorOfIterators) -> Iterator when
      IteratorOfIterators :: iterator(iterator()),
      Iterator :: iterator().
merge(#iterator{} = IteratorOfIterators) ->
    fmerge(to_list(IteratorOfIterators)).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted before
%% evaluating this function.  When two elements compare equal, the
%% element from `Iterator1' is picked before the element from
%% `Iterator2'.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec merge(Iterator1, Iterator2) -> Iterator3 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(A | B).
merge(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    fmerge([Iterator1, Iterator2]).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted
%% according to the ordering function `Fun' before evaluating this
%% function. `Fun(A, B)' is to return `true' if `A' compares less than
%% or equal to `B' in the ordering, otherwise `false'. When two
%% elements compare equal, the element from `Iterator1' is picked
%% before the element from `Iterator2'.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec merge(Fun, Iterator1, Iterator2) -> Iterator3 when
      Fun :: compare(A, B),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(A | B).
merge(Fun, #iterator{} = Iterator1, #iterator{} = Iterator2) ->
    fmerge(Fun, [Iterator1, Iterator2]).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1',
%% `Iterator2', and `Iterator3'.  All of `Iterator1', `Iterator2', and
%% `Iterator3' must be sorted before evaluating this function. When
%% two elements compare equal, the element from `Iterator1', if there
%% is such an element, is picked before the other element, otherwise
%% the element from `Iterator2' is picked before the element from
%% `Iterator3'.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec merge3(Iterator1, Iterator2, Iterator3) -> Iterator4 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(C),
      Iterator4 :: iterator(A | B | C).
merge3(#iterator{} = Iterator1,
       #iterator{} = Iterator2,
       #iterator{} = Iterator3) ->
    fmerge([Iterator1, Iterator2, Iterator3]).

%% @doc
%% Returns the `N'th tail of `Iterator1', that is, the subiterator of
%% `Iterator1' starting at `N'+1 and continuing up to the end of the
%% iterator.
%% @end
-spec nthtail(N, Iterator1) -> Iterator2 when
      N :: non_neg_integer(),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
nthtail(N, #iterator{} = Iterator) when is_integer(N), N >= 0 ->
    nthtail_loop(N, Iterator).

%% @doc
%% Returns an iterator with the elements in `Iterator1' in reverse
%% order.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec reverse(Iterator1) -> Iterator2 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(A).
reverse(#iterator{} = Iterator) ->
    list_wrap(fun lists:reverse/1, Iterator).

%% @doc
%% Returns a list with the elements in `Iterator1' in reverse order,
%% with tail `TailIterator' appended.
%%
%% Example:
%% ```
%% > lists:reverse([1, 2, 3, 4], [a, b, c]).
%% [4,3,2,1,a,b,c]
%% '''
%%
%% The iterator `Iterator1' will be fully evaluated, infinite
%% iterators will never return.
%% @end
-spec reverse(Iterator1, TailIterator) -> Iterator2 when
      Iterator1 :: iterator(A),
      TailIterator :: iterator(B),
      Iterator2 :: iterator(A | B).
reverse(#iterator{} = Iterator, #iterator{} = Tail) ->
    append(reverse(Iterator), Tail).

%% @see sublist/3
-spec sublist(Iterator1, Len) -> Iterator2 when
      Iterator1 :: iterator(Elem),
      Len :: non_neg_integer(),
      Iterator2 :: iterator(Elem).
sublist(#iterator{} = Iterator, Len) when is_integer(Len), Len >= 0->
    new(fun () ->
                case {Len, next(Iterator)} of
                    {0, _} ->
                        [];
                    {_, []} ->
                        [];
                    {Len, [Elem | #iterator{} = NextIterator]} ->
                        [Elem | sublist(NextIterator, Len - 1)]
                end
        end).

%% @doc
%% Returns the portion of `Iterator1' starting at `Start' and with
%% (maximum) `Len' elements. `Start' defaults to 1. It is not an error
%% for `Start+Len' to exceed the length of the iterator, in that case
%% the whole iterator is returned.
%% @end
-spec sublist(Iterator1, Start, Len) -> Iterator2 when
      Iterator1 :: iterator(Elem),
      Start :: pos_integer(),
      Len :: non_neg_integer(),
      Iterator2 :: iterator(Elem).
sublist(#iterator{} = Iterator, Start, Len) when
      is_integer(Start),
      is_integer(Len),
      Start > 0,
      Len >= 0 ->
    sublist(nthtail(Start - 1, Iterator), Len).

%% @doc
%% Takes elements `Elem' from `Iterator1' while `Pred(Elem)' returns
%% true, that is, the function returns the longest prefix of the
%% iterator for which all elements satisfy the predicate.
%% @end
-spec takewhile(Pred, Iterator1) -> Iterator2 when
      Pred :: predicate(Elem),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
takewhile(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    new(fun () ->
                case next(Iterator) of
                    [Elem | #iterator{} = NextIterator] ->
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
%% Partitions `Iterator1' into two iterators, where the first iterator
%% contains all elements for which `Pred(Elem)' returns `true', and
%% the second iterator contains all elements for which `Pred(Elem)'
%% returns `false'.
%%
%% Examples:
%% ```
%% > {Satisfying, NotSatisfying} = llists:partition(
%% >  fun(A) -> A rem 2 == 1 end,
%% >  llists:seq(1, 7)),
%% > {llists:to_list(Satisfying), llists:to_list(NotSatisfying)}.
%% {[1,3,5,7],[2,4,6]}
%% > {Satisfying, NotSatisfying} = llists:partition(
%% >  fun(A) -> is_atom(A) end,
%% >  llists:from_list([a,b,1,c,d,2,3,4,e])),
%% > {llists:to_list(Satisfying), llists:to_list(NotSatisfying)}.
%% {[a,b,c,d,e],[1,2,3,4]}
%% '''
%%
%% For a different way to partition a list, see splitwith/2.
%%
%% Each result iterator will evaluate elements of the original
%% iterator independently. If both are evaluated, this will result in
%% all elements being evaluated twice.
%% @end
%% @see splitwith/2
-spec partition(Pred, Iterator1) -> {Satisfying, NotSatisfying} when
      Pred :: predicate(Elem),
      Iterator1 :: iterator(Elem),
      Satisfying :: iterator(Elem),
      NotSatisfying :: iterator(Elem).
partition(Pred, #iterator{} = Iterator) ->
    Satisfying = filter(Pred, Iterator),
    NotSatisfying = filter(fun (Elem) -> not Pred(Elem) end, Iterator),
    {Satisfying, NotSatisfying}.

%% @doc
%% Returns an iterator containing the sorted elements of `Iterator1'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec sort(Iterator1) -> Iterator2 when
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
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
-spec sort(Fun, Iterator1) -> Iterator2 when
      Fun :: compare(A, B),
      B :: A,
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(A).
sort(Fun, #iterator{} = Iterator) when is_function(Fun, 2) ->
    list_wrap(fun (I) -> lists:sort(Fun, I) end, Iterator).

%% @doc
%% Splits `Iterator1' into `Iterator2' and `Iterator3'. `Iterator2'
%% contains the first `N' elements and `Iterator3' the remaining
%% elements (the `N'th tail).
%%
%% Evaluates the first `N' elements of `Iterator1' to construct
%% `Iterator3'.
%% @end
-spec split(N, Iterator1) -> {Iterator2, Iterator3} when
      N :: non_neg_integer(),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem),
      Iterator3 :: iterator(Elem).
split(N, #iterator{} = Iterator) when is_integer(N), N >= 0 ->
    {sublist(Iterator, N), nthtail(N, Iterator)}.

%% @doc
%% Partitions `Iterator1' into two iterators according to `Pred'.
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
-spec splitwith(Pred, Iterator1) -> {Iterator2, Iterator3} when
      Pred :: predicate(Elem),
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem),
      Iterator3 :: iterator(Elem).
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
-spec subtract(Iterator1, Iterator2) -> Iterator3 when
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(),
      Iterator3 :: iterator(Elem).
subtract(#iterator{} = BaseIterator, #iterator{} = RemoveIterator) ->
    unfold(fun Next({#iterator{} = FoldIterator, Remove}) ->
                   case next(FoldIterator) of
                       [] ->
                           none;
                       [Elem | #iterator{} = NextIterator] ->
                           case drop(Elem, Remove) of
                               none ->
                                   {Elem, {NextIterator, Remove}};
                               {dropped, NewRemove} ->
                                   Next({NextIterator, NewRemove})
                           end
                   end
           end,
           {BaseIterator, to_list(RemoveIterator)}).

%% @doc
%% Returns the sorted iterator formed by merging `TupleIterator1' and
%% `TupleIterator2'. The merge is performed on the `N'th element of each
%% tuple. Both `TupleIterator1' and `TupleIterator2' must be key-sorted without
%% duplicates before evaluating this function. When two tuples compare
%% equal, the tuple from `TupleIterator1' is picked and the one from
%% `TupleIterator2' is deleted.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec ukeymerge(N, TupleIterator1, TupleIterator2) -> TupleIterator3 when
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem1),
      TupleIterator2 :: iterator(Elem2),
      TupleIterator3 :: iterator(Elem1 | Elem2),
      Elem1 :: tuple(),
      Elem2 :: tuple().
ukeymerge(N, #iterator{} = Iterator1, #iterator{} = Iterator2) ->
    Equal = fun (A, B) -> element(N, A) == element(N, B) end,
    Compare = fun (A, B) -> element(N, A) =< element(N, B) end,
    ufmerge(Equal, Compare, [Iterator1, Iterator2]).

%% @doc
%% Returns a iterator containing the sorted elements of iterator
%% `TupleIterator1' where all except the first tuple of the tuples
%% comparing equal have been deleted. Sorting is performed on the
%% `N'th element of the tuples.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec ukeysort(N, TupleIterator1) -> TupleIterator2 when
      N :: pos_integer(),
      TupleIterator1 :: iterator(Elem),
      TupleIterator2 :: iterator(Elem),
      Elem :: tuple().
ukeysort(N, #iterator{} = Iterator) when N > 0 ->
    list_wrap(fun (I) -> lists:ukeysort(N, I) end, Iterator).

%% @doc
%% Returns the sorted iterator formed by merging all the subiterators of
%% `IteratorOfIterators'. All subiterators must be sorted and contain no duplicates
%% before evaluating this function. When two elements compare equal,
%% the element from the subiterator with the lowest position in
%% `IteratorOfIterators' is picked and the other is deleted.
%%
%% The first element of each subiterator will be evaluated.
%% @end
-spec umerge(IteratorOfIterators) -> Iterator when
      IteratorOfIterators :: iterator(iterator()),
      Iterator :: iterator().
umerge(#iterator{} = IteratorOfIterators) ->
    ufmerge(to_list(IteratorOfIterators)).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted and
%% contain no duplicates before evaluating this function. When two
%% elements compare equal, the element from `Iterator1' is picked and
%% the one from `Iterator2' is deleted.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec umerge(Iterator1, Iterator2) -> Iterator3 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(A | B).
umerge(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    ufmerge([Iterator1, Iterator2]).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1' and
%% `Iterator2'. Both `Iterator1' and `Iterator2' must be sorted
%% according to the ordering function `Fun' and contain no duplicates
%% before evaluating this function. `Fun(A, B)' is to return `true' if
%% `A' compares less than or equal to `B' in the ordering, otherwise
%% `false'. When two elements compare equal, the element from
%% `Iterator1' is picked and the one from `Iterator2' is deleted.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec umerge(Fun, Iterator1, Iterator2) -> Iterator3 when
      Fun :: compare(A, B),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(A | B).
umerge(Fun, #iterator{} = Iterator1, #iterator{} = Iterator2) ->
    ufmerge(fun (A, B) -> Fun(A, B) andalso Fun(B, A) end,
            Fun,
            [Iterator1, Iterator2]).

%% @doc
%% Returns the sorted iterator formed by merging `Iterator1',
%% `Iterator2', and `Iterator3'.  All of `Iterator1', `Iterator2', and
%% `Iterator3' must be sorted and contain no duplicates before
%% evaluating this function. When two elements compare equal, the
%% element from `Iterator1' is picked if there is such an element,
%% otherwise the element from `Iterator2' is picked, and the other is
%% deleted.
%%
%% The first element of each iterator will be evaluated.
%% @end
-spec umerge3(Iterator1, Iterator2, Iterator3) -> Iterator4 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(C),
      Iterator4 :: iterator(A | B | C).
umerge3(#iterator{} = Iterator1,
        #iterator{} = Iterator2,
        #iterator{} = Iterator3) ->
    ufmerge([Iterator1, Iterator2, Iterator3]).

%% @doc
%% "Unzips" a iterator of two-tuples into two iterators, where the
%% first iterator contains the first element of each tuple, and the
%% second iterator contains the second element of each tuple.
%% @end
-spec unzip(Iterator1) -> {Iterator2, Iterator3} when
      Iterator1 :: iterator({A, B}),
      Iterator2 :: iterator(A),
      Iterator3 :: iterator(B).
unzip(#iterator{} = Iterator) ->
    {map(fun ({A, _}) -> A end, Iterator),
     map(fun ({_, B}) -> B end, Iterator)}.

%% @doc
%% "Unzips" a iterator of three-tuples into three iterators, where the first
%% iterator contains the first element of each tuple, the second iterator
%% contains the second element of each tuple, and the third iterator
%% contains the third element of each tuple.
%% @end
-spec unzip3(Iterator1) -> {Iterator2, Iterator3, Iterator4} when
      Iterator1 :: iterator({A, B, C}),
      Iterator2 :: iterator(A),
      Iterator3 :: iterator(B),
      Iterator4 :: iterator(C).
unzip3(#iterator{} = Iterator) ->
    {map(fun ({A, _, _}) -> A end, Iterator),
     map(fun ({_, B, _}) -> B end, Iterator),
     map(fun ({_, _, C}) -> C end, Iterator)}.

%% @doc
%% Returns a iterator containing the sorted elements of `Iterator1'
%% where all except the first element of the elements comparing equal
%% have been deleted.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec usort(Iterator1) -> Iterator2 when
      Iterator1 :: iterator(Elem),
      Iterator2 :: iterator(Elem).
usort(#iterator{} = Iterator) ->
    list_wrap(fun lists:usort/1, Iterator).

%% @doc
%% Returns a iterator containing the sorted elements of `Iterator1' where all
%% except the first element of the elements comparing equal according
%% to the ordering function `Fun' have been deleted. `Fun(A, B)' is to
%% return `true' if `A' compares less than or equal to `B' in the ordering,
%% otherwise `false'.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec usort(Fun, Iterator1) -> Iterator2 when
      Fun :: compare(A, B),
      B :: A,
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(A).
usort(Fun, #iterator{} = Iterator) when is_function(Fun, 2) ->
    list_wrap(fun (I) -> lists:usort(Fun, I) end, Iterator).

%% @doc
%% "Zips" two iterators of equal length into one iterator of
%% two-tuples, where the first element of each tuple is taken from the
%% first iterator and the second element is taken from the
%% corresponding element in the second iterator.
%% @end
-spec zip(Iterator1, Iterator2) -> Iterator3 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator({A, B}).
zip(#iterator{} = Iterator1, #iterator{} = Iterator2) ->
    zipwith(fun (A, B) -> {A, B} end, Iterator1, Iterator2).

%% @doc
%% "Zips" three iterators of equal length into one iterator of
%% three-tuples, where the first element of each tuple is taken from
%% the first iterator, the second element is taken from the
%% corresponding element in the second iterator, and the third element
%% is taken from the corresponding element in the third iterator.
%% @end
-spec zip3(Iterator1, Iterator2, Iterator3) -> Iterator4 when
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(C),
      Iterator4 :: iterator({A, B, C}).
zip3(#iterator{} = Iterator1, #iterator{} = Iterator2, #iterator{} = Iterator3) ->
    zipwith3(fun (A, B, C) -> {A, B, C} end, Iterator1, Iterator2, Iterator3).

%% @doc
%% Combines the elements of two iterators of equal length into one iterator.
%% For each pair `X, Y' of iterator elements from the two iterators, the element
%% in the result iterator is `Combine(X, Y)'.
%%
%% `llists:zipwith(fun(X, Y) -> {X, Y} end, Iterator1, Iterator2)' is
%% equivalent to `llists:zip(Iterator1, Iterator2)'.
%%
%% Example:
%% ```
%% > llists:to_list(
%% >  llists:zipwith(fun(X, Y) -> X + Y end, llists:seq(1, 3), llist:seq(4, 6))).
%% [5,7,9]
%% '''
%% @end
-spec zipwith(Combine, Iterator1, Iterator2) -> Iterator3 when
      Combine :: combine(X, Y, Out),
      Iterator1 :: iterator(X),
      Iterator2 :: iterator(Y),
      Iterator3 :: iterator(Out).
zipwith(Combine, #iterator{} = Iterator1, #iterator{} = Iterator2) when
      is_function(Combine, 2) ->
    new(fun () ->
                case {next(Iterator1), next(Iterator2)} of
                    {[Elem1 | #iterator{} = NextIterator1],
                     [Elem2 | #iterator{} = NextIterator2]} ->
                        [Combine(Elem1, Elem2) |
                         zipwith(Combine, NextIterator1, NextIterator2)];
                    {[], []} ->
                        [];
                    _ ->
                        % Because that's how lists:zip* crashes on
                        % unequal length lists.
                        error(function_clause)
                end
        end).

%% @doc
%% Combines the elements of three iterators of equal length into one
%% iterator. For each triple `X, Y, Z' of iterator elements from the
%% three iterators, the element in the result iterator is 
%% `Combine(X, Y, Z)'.
%%
%% `zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, Iterator1, Iterator2, Iterator3)'
%% is equivalent to `zip3(Iterator1, Iterator2, Iterator3)'.
%%
%% Examples:
%% ```
%% > llists:to_list(
%% >  llists:zipwith3(
%% >   fun(X, Y, Z) -> X + Y + Z end,
%% >   llists:seq(1, 3),
%% >   llists:seq(4, 6),
%% >   llists:seq(7, 9))).
%% [12,15,18]
%% > llists:to_list(
%% >  llists:zipwith3(
%% >   fun(X, Y, Z) -> [X, Y, Z] end,
%% >   llists:from_list([a,b,c]),
%% >   llists:from_list([x,y,z]),
%% >   llists:seq(1, 3))).
%% [[a,x,1],[b,y,2],[c,z,3]]
%% '''
%% @end
-spec zipwith3(Combine, Iterator1, Iterator2, Iterator3) -> Iterator4 when
      Combine :: combine3(A, B, C, Out),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B),
      Iterator3 :: iterator(C),
      Iterator4 :: iterator(Out).
zipwith3(Combine,
         #iterator{} = Iterator1,
         #iterator{} = Iterator2,
         #iterator{} = Iterator3) when 
      is_function(Combine, 3) ->
    new(fun () ->
                case {next(Iterator1), next(Iterator2), next(Iterator3)} of
                    {[Elem1 | #iterator{} = NextIterator1],
                     [Elem2 | #iterator{} = NextIterator2],
                     [Elem3 | #iterator{} = NextIterator3]} ->
                        [Combine(Elem1, Elem2, Elem3) |
                         zipwith3(Combine,
                                  NextIterator1,
                                  NextIterator2,
                                  NextIterator3)];
                    {[], [], []} ->
                        [];
                    _ ->
                        % Because that's how lists:zip* crashes on
                        % unequal length lists.
                        error(function_clause)
                end
        end).

%%%===================================================================
%%% API - Iterator Evaluation
%%%===================================================================

%% @doc
%% Fully evaluate `Iterator' and return a list containing all elements
%% produced. Infinite iterators will never return.
%% @end
-spec to_list(Iterator) -> List when
      Iterator :: iterator(Elem),
      List :: [Elem].
to_list(#iterator{} = Iterator) ->
    to_list_loop(next(Iterator)).

%% @doc
%% Fully evaluate an `Iterator' of `{Key, Value}' tuples and return a
%% map containing all pairs produced. Infinite iterators will never
%% return.
%%
%% If duplicate `Key's are present in the iterator it is undefined
%% which will appear in the final map.
%% @end
-spec to_map(Iterator) -> Map when
      Iterator :: iterator({Key, Value}),
      Key :: any(),
      Value :: any(),
      Map :: maps:map(Key, Value).
to_map(#iterator{} = Iterator) ->
    to_map_loop(next(Iterator), #{}).

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
-spec length(Iterator) -> Length when
      Iterator :: iterator(),
      Length :: non_neg_integer().
length(#iterator{} = Iterator) ->
    foldl(fun (_, Acc) -> Acc + 1 end, 0, Iterator).

%% @doc
%% Returns `true' if `Pred(Elem)' returns `true' for all elements
%% `Elem' in `Iterator'.
%% 
%% Stops evaluating `Iterator' when `Pred(Elem)' returns `false' or
%% when `Iterator' is empty.
%% @end
-spec all(Pred, Iterator) -> boolean() when
      Pred :: predicate(Elem),
      Iterator :: iterator(Elem).
all(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    all_loop(Pred, next(Iterator)).

%% @doc
%% Returns `true' if `Pred(Elem)' returns `true' for at least one
%% element `Elem' in `Iterator'. 
%%
%% Stops evaluating `Iterator' when `Pred(Elem)' returns `true' or
%% when `Iterator' is empty.
%% @end
-spec any(Pred, Iterator) -> boolean() when
      Pred :: predicate(Elem),
      Iterator :: iterator(Elem).
any(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    any_loop(Pred, next(Iterator)).

%% @doc
%% Concatenates the text representation of the elements of `Iterator'.
%% The elements of `Iterator' can be atoms, integers, floats, or
%% strings. The iterator will be fully evaluated, infinite iterators
%% will never return.
%% @end
-spec concat(Iterator) -> string() when
      Iterator :: iterator(atom() | integer() | float() | string()).
concat(#iterator{} = Iterator) ->
    lists:concat(to_list(Iterator)).

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
-spec foldl(Fun, Acc0, Iterator) -> AccOut when
      Fun :: fold(A, AccIn :: Acc0 | AccOut, AccOut),
      Acc0 :: any(),
      Iterator :: iterator(A).
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
-spec foldr(Fun, Acc0, Iterator) -> AccOut when
      Fun :: fold(A, AccIn :: Acc0 | AccOut, AccOut),
      Acc0 :: any(),
      Iterator :: iterator(A).
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
-spec foreach(Fun, Iterator) -> ok when
      Fun :: map(Elem, any()),
      Iterator :: iterator(Elem).
foreach(Fun, #iterator{} = Iterator) when is_function(Fun, 1) ->
    foreach_loop(Fun, next(Iterator)).

%% @doc
%% Searches the iterator of tuples `TupleIterator' for a tuple whose
%% `N'th element compares equal to `Key'. Returns `Tuple' if such a
%% tuple is found, otherwise `false'.
%%
%% The iterator will be evaluated until a match is found. If no match
%% is found, infinite iterators will never return.
%% @end
-spec keyfind(Key, N, TupleIterator) -> Tuple | false when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator :: iterator(),
      Tuple :: tuple() | false.
keyfind(Key, N, #iterator{} = Iterator) when N > 0 ->
    Found = search(fun (Elem) when element(N, Elem) == Key -> true;
                       (_) -> false end,
                   Iterator),
    case Found of
        {value, Value} when is_tuple(Value) ->
            Value;
        false ->
            false
    end.

%% @doc
%% Returns `true' if there is a tuple in `TupleIterator' whose `N'th
%% element compares equal to `Key', otherwise `false'.
%%
%% The iterator will be evaluated until a match is found. If no match
%% is found, infinite iterators will never return.
%% @end
-spec keymember(Key, N, TupleIterator) -> boolean() when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator :: iterator().
keymember(Key, N, #iterator{} = Iterator) when N > 0 ->
    any(fun (Elem) when element(N, Elem) == Key -> true;
            (_) -> false
        end,
        Iterator).

%% @doc
%% Searches the iterator of tuples `TupleIterator' for a tuple whose
%% `N'th element compares equal to `Key'. Returns `{value, Tuple}' if
%% such a tuple is found, otherwise `false'.
%%
%% Function keyfind/3 is usually more convenient.
%% @end
%% @see keyfind/3
-spec keysearch(Key, N, TupleIterator) -> {value, Tuple} | false when
      Key :: any(),
      N :: pos_integer(),
      TupleIterator :: iterator(),
      Tuple :: tuple().
keysearch(Key, N, #iterator{} = Iterator) ->
    search(fun (Elem) when element(N, Elem) == Key -> true;
               (_) -> false end,
           Iterator).

%% @doc
%% Returns the last element in `Iterator'.
%% 
%% The iterator will be fully evaluated, infinite iterators will never
%% return.
%% @end
-spec last(Iterator) -> Elem when
      Iterator :: iterator(Elem).
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
%% ```
%% > % Summing the elements in an iterator and double them at the same time:
%% > DoubleAndSum = fun(X, Sum) -> {2*X, X+Sum} end,
%% > {Mapped, Acc} = llists:mapfoldl(DoubleAndSum, 0, llists:seq(1,5)),
%% > {llists:to_list(Mapped), Acc}.
%% {[2,4,6,8,10],15}
%% '''
%%
%% The iterator is fully evaluated before the mapfold begins, infinite
%% iterators will never return.
%% @end
-spec mapfoldl(Fun, Acc0, Iterator1) -> {Iterator2, AccOut} when
      Fun :: mapfold(A, AccIn :: Acc0 | AccOut, B, AccOut),
      Acc0 :: accumulator(),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B).
mapfoldl(Fun, Acc0, #iterator{} = Iterator) ->
    {Mapped, AccOut} = lists:mapfoldl(Fun, Acc0, to_list(Iterator)),
    {llists:from_list(Mapped), AccOut}.

%% @doc
%% Combines the operations of map/2 and foldr/3 into one pass.
%%
%% The iterator is fully evaluated before the mapfold begins, infinite
%% iterators will never return.
%% @end
-spec mapfoldr(Fun, Acc0, Iterator1) -> {Iterator2, AccOut} when
      Fun :: mapfold(A, AccIn :: Acc0 | AccOut, B, AccOut),
      Acc0 :: accumulator(),
      Iterator1 :: iterator(A),
      Iterator2 :: iterator(B).
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
-spec max(Iterator) -> Elem when
      Iterator :: iterator(Elem).
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
-spec member(Elem, Iterator) -> boolean() when
      Elem :: any(),
      Iterator :: iterator().
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
-spec min(Iterator) -> Elem when
      Iterator :: iterator(Elem).
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
-spec nth(N, Iterator) -> Elem when
      N :: pos_integer(),
      Iterator :: iterator(Elem).
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
-spec prefix(Iterator1, Iterator2) -> boolean() when
      Iterator1 :: iterator(),
      Iterator2 :: iterator().
prefix(#iterator{} = Prefix, #iterator{} = Iterator) ->
    prefix_loop(next(Prefix), next(Iterator)).

%% @doc
%% If there is a `Value' in `Iterator' such that `Pred(Value)' returns `true',
%% returns `{value, Value}' for the first such `Value', otherwise returns
%% `false'.
%%
%% The iterator is evaluated until a match is found. If no match is
%% ever found, infinite iterators will never return.
%% @end
-spec search(Pred, Iterator) -> {value, Value} | false when
      Pred :: predicate(Value),
      Iterator :: iterator().
search(Pred, #iterator{} = Iterator) when is_function(Pred, 1) ->
    search_loop(Pred, next(Iterator)).

%% @doc
%% Returns `true' if `Iterator1' is a suffix of `Iterator2', otherwise
%% `false'.
%%
%% Both `Iterator1' and `Iterator2' are fully evaluated, infinite
%% iterators will never return.
%% @end
-spec suffix(Iterator1, Iterator2) -> boolean() when
      Iterator1 :: iterator(),
      Iterator2 :: iterator().
suffix(#iterator{} = Suffix, #iterator{} = Iterator) ->
    prefix(reverse(Suffix), reverse(Iterator)).

%% @doc
%% Returns the sum of the elements in `Iterator'.
%%
%% The iterator is fully evaluated, infinite iterators will never
%% return.
%% @end
-spec sum(Iterator) -> Sum when
      Iterator :: iterator(Elem),
      Sum :: Elem,
      Elem :: number().
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

%% @private
%% @doc
%% Attempts to remove `Elem' from the list `List1'. If it is found
%% returns the list without the removed value as `{dropped, List2}',
%% otherwise returns `none'.
%% @end
drop(Elem, List) ->
    case List -- [Elem] of
        List ->
            none;
        Dropped ->
            {dropped, Dropped}
    end.

%% @private
%% @see fmerge/2
fmerge(IteratorOfIterators) ->
    fmerge(fun (A, B) -> A =< B end, IteratorOfIterators).

%% @private
%% @doc
%% Merge a list of iterators according to an ordering function which
%% returns `true' when the first element is less than or equal to the
%% second, `false' otherwise. All iterators are expected to be already
%% ordered.
%%
%% This preserves the `lists' invariant that the leftmost iterator is
%% chosen when values are equal. It is not able to replicate the
%% behaviour of merge when given (invalid) unordered lists. Divergence
%% appears to be easiest to replicate with input like:
%% `[[0], [0], [0], [1, 0]]'.
%%
%% The name comes from the internals of the lists module, which
%% delegates to a similarly named function to handle merging with an
%% ordering function.
%% @end
fmerge(Compare, ListOfIterators) when is_function(Compare, 2) ->
    LazyLists = [Next || Iterator <- ListOfIterators,
                         Next <- [next(Iterator)],
                         Next /= []],
    unfold(fun ([]) ->
                   none;
               ([_ | _] = Lists) ->
                   {Smallest, {Found,
                               FoundBefore,
                               FoundAfter}} = fmerge_sort(Compare, Lists),
                   {Smallest, fmerge_next(Found, FoundBefore, FoundAfter)}
           end,
           lists:reverse(LazyLists)).

fmerge_compare(Fun, [Elem1 | _], [Elem2 | _]) ->
    Fun(Elem1, Elem2).

fmerge_next(Iterator, Before, After) ->
    case next(Iterator) of
        [] ->
            Before ++ After;
        [_ | _] = Next ->
            Before ++ [Next | After]
    end.

fmerge_sort(Fun, [Smallest | FoundAfter] = After) ->
    fmerge_sort(Fun, [], After, {Smallest, [], FoundAfter}).

fmerge_sort(_Fun, _Before, [], {[Smallest | Iterator], FoundBefore, FoundAfter}) ->
    {Smallest, {Iterator, lists:reverse(FoundBefore), FoundAfter}};
fmerge_sort(Fun, Before, [Compare | After], {Smallest, _, _} = Found) ->
    case fmerge_compare(Fun, Compare, Smallest) of
        true ->
            fmerge_sort(Fun, [Compare | Before], After, {Compare, Before, After});
        false ->
            fmerge_sort(Fun, [Compare | Before], After, Found)
    end.

%% @private
%% @see ufmerge/3
ufmerge(IteratorOfIterators) ->
    ufmerge(fun (A, B) -> A == B end,
           fun (A, B) -> A =< B end,
           IteratorOfIterators).

%% @private
%% @doc
%% Merge a list of iterators according to an ordering function which
%% returns `true' when the first element is less than or equal to the
%% second, `false' otherwise. Discards duplicates across iterators
%% according to an equality function. All iterators are expected to be
%% already ordered and without duplicates.
%%
%% This preserves the `lists' invariant that the leftmost iterator is
%% chosen when values are equal. It is not able to replicate the
%% behaviour of merge when given (invalid) unordered lists or
%% (invalid) lists with duplicates. Divergence appears to be easiest
%% to replicate with input like:
%% `[[0, 0], [0, 0]]'.
%%
%% It was very tempting to instead name this fumerge, as replicating
%% undocumented merge behaviour from the `lists' module has proven to
%% be far more difficult than expected.
%% @end
ufmerge(Equal, Compare, ListOfIterators) when
      is_function(Equal, 2), is_function(Compare, 2) ->
    LazyLists = [Next || Iterator <- ListOfIterators,
                         Next <- [next(Iterator)],
                         Next /= []],
    unfold(fun ([]) ->
                   none;
               ([_ | _] = Lists) ->
                   {Smallest, {Found,
                               FoundBefore,
                               FoundAfter}} = fmerge_sort(Compare, Lists),
                   UniqueBefore = ufmerge_skip(Equal, Smallest, FoundBefore),
                   UniqueAfter = ufmerge_skip(Equal, Smallest, FoundAfter),
                   {Smallest, fmerge_next(Found, UniqueBefore, UniqueAfter)}
           end,
           lists:reverse(LazyLists)).

ufmerge_skip(Equal, Smallest, LazyLists) ->
    [LazyList ||
     [Elem | Iterator] <- LazyLists,
     LazyList <- [case Equal(Elem, Smallest) of
                      true ->
                          next(Iterator);
                      false ->
                          [Elem | Iterator]
                  end],
     LazyList /= []].

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

to_map_loop([], Acc) ->
    Acc;
to_map_loop([{Key, Value} | #iterator{} = Iterator], Acc) ->
    to_map_loop(next(Iterator), Acc#{Key=>Value}).

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
