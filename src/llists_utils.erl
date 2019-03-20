%%%-------------------------------------------------------------------
%%% @doc
%%% Additional iterator utilities that are not replicas of `lists'
%%% module functionality. These functions are kept separate to avoid
%%% any future name clashes with additions to the stdlib.
%%% @end
%%%-------------------------------------------------------------------
-module(llists_utils).

%% API
-export([cycle/1,
         group/2,
         groupwith/2,
         unique/1,
         unique/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Create an infinite iterator that repeatedly returns the sequence of
%% elements in the given iterator.
%% @end
-spec cycle(Iterator1) -> Iterator2 when
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator(Elem).
cycle(Iterator) ->
    true = llists:is_iterator(Iterator),
    llists:append(llists:duplicate(infinity, Iterator)).

%% @doc
%% Create an iterator that returns groups of elements from `Iterator1'
%% as a list of at least `Length' elements.
%%
%% It is not an error if there are not enough elements to fill out the
%% final group, instead a smaller group is returned.
%% @end
-spec group(Length, Iterator1) -> Iterator2 when
      Length :: pos_integer(),
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator([Elem]).
group(Length, Iterator) when Length > 0 ->
    true = llists:is_iterator(Iterator),
    llists:unfold(fun (FoldIterator) ->
                          group_unfold(Length, [], FoldIterator)
                  end,
                  Iterator).

%% @doc
%% Create an iterator that returns groups of elements from `Iterator1'
%% based on the return value of `Pred(Elem)'. If the predicate
%% function returns `true' it signals the end of a group which will be
%% returned as a list. If the predicate returns `false', the element
%% will be included in the next group returned. Even if the predicate
%% function returns `false' for the last element, the final group will
%% still be returned.
%%
%% If `Pred(Elem)' returns false for every element in an infinite
%% iterator, the first evaluation of `Iterator2' will never return.
%% @end
-spec groupwith(Pred, Iterator1) -> Iterator2 when
      Pred :: llists:predicate(Elem),
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator([Elem]).
groupwith(Pred, Iterator) when is_function(Pred, 1) ->
    true = llists:is_iterator(Iterator),
    llists:unfold(fun (FoldIterator) ->
                          groupwith_unfold(Pred, [], FoldIterator)
                  end,
                  Iterator).

%% @doc
%% As `unique/2', but with `==' as a equality function.
%% @end
%% @see unique/2
-spec unique(Iterator1) -> Iterator2 when
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator(Elem).
unique(Iterator) ->
    true = llists:is_iterator(Iterator),
    unique(fun erlang:'=='/2, Iterator).

%% @doc
%% Discards repeated values in a sorted iterator according to a
%% provided equality function `Fun(A, B)' which should return `true'
%% when `A' and `B' are equal and `false' otherwise. All values that
%% compares equal to the previously returned value are skipped until a
%% non-equal value is found.
%%
%% Infinite iterators of equal values will cause the first evaluation
%% of `Iterator2' to never return.
%% @end
-spec unique(Fun, Iterator1) -> Iterator2 when
      Fun :: llists:compare(A, B),
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator(Elem),
      A :: Elem,
      B :: Elem.
unique(Fun, Iterator) when is_function(Fun, 2) ->
    true = llists:is_iterator(Iterator),
    llists:unfold(fun Next({_Prev, []}) ->
                   none;
               Next({Prev, FoldIterator}) ->
                   case {Prev, llists:next(FoldIterator)}  of
                       {_Prev, []} ->
                           none;
                       {first, [Elem | NextIterator]} ->
                           {Elem, {{previous, Elem}, NextIterator}};
                       {{previous, PrevElem} = Prev,
                        [Elem | NextIterator]} ->
                           case Fun(Elem, PrevElem) of
                               true ->
                                   Next({Prev, NextIterator});
                               false ->
                                   {Elem, {{previous, Elem}, NextIterator}}
                           end
                   end
           end,
           {first, Iterator}).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

group_unfold(_N, _Acc, none) ->
    none;
group_unfold(0, Acc, Iterator) ->
    {lists:reverse(Acc), Iterator};
group_unfold(N, Acc, Iterator) when N > 0 ->
    case {Acc, llists:next(Iterator)} of
        {[], []} ->
            none;
        {_, []} ->
            {lists:reverse(Acc), none};
        {_, [Elem | NextIterator]} ->
            group_unfold(N - 1, [Elem | Acc], NextIterator)
    end.

groupwith_unfold(_Pred, _Acc, none) ->
    none;
groupwith_unfold(Pred, Acc, Iterator) ->
    case {Acc, llists:next(Iterator)} of
        {[], []} ->
            none;
        {_, []} ->
            {lists:reverse(Acc), none};
        {_, [Elem | NextIterator]} ->
            case Pred(Elem) of
                true ->
                    {lists:reverse([Elem | Acc]), NextIterator};
                false ->
                    groupwith_unfold(Pred, [Elem | Acc], NextIterator)
            end
    end.
