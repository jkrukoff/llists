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
    llists:append(llists:duplicate(infinity, Iterator)).

%% @doc
%% As `unique/2', but with `==' as a equality function.
%% @end
%% @see unique/2
-spec unique(Iterator1) -> Iterator2 when
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator(Elem).
unique(Iterator) ->
    unique(fun erlang:'=='/2, Iterator).

%% @doc
%% Discards repeated values in a sorted iterator according to a
%% provided equality function `Fun(A, B)' which should return `true'
%% when `A' and `B' are equal and `false' otherwise. All values that
%% compares equal to the previously returned value are skipped until a
%% non-equal value is found.
%%
%% Infinite iterators of equal values will never return.
%% @end
-spec unique(Fun, Iterator1) -> Iterator2 when
      Fun :: llists:compare(A, B),
      Iterator1 :: llists:iterator(Elem),
      Iterator2 :: llists:iterator(Elem),
      A :: Elem,
      B :: Elem.
unique(Fun, Iterator) when is_function(Fun, 2) ->
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

