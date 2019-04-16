%%%-------------------------------------------------------------------
%%% @doc
%%% Additional iterator utilities that are not replicas of `lists'
%%% module functionality. These functions are kept separate to avoid
%%% any future name clashes with additions to the stdlib.
%%% @end
%%%-------------------------------------------------------------------
-module(llists_utils).

-record(zipper, {heads, tail}).

-type permutation_options() :: proplists:proplist().

%% API
-export([combinations/2,
         combinations/3,
         cycle/1,
         group/2,
         groupwith/2,
         permutations/2,
         permutations/3,
         unique/1,
         unique/2]).

-export_type([permutation_options/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @see combinations/3
-spec combinations(N, Choices) -> Iterator when
      N :: non_neg_integer(),
      Choices :: [Elem],
      Iterator :: llists:iterator([Elem]).
combinations(N, Choices) when N >= 0, is_list(Choices) ->
    llists:unfold(fun (none) ->
                          none;
                      (CurrentChoices) ->
                          NextChoice = next_choice(CurrentChoices),
                          NextChoices = next_combination(CurrentChoices),
                          {NextChoice, NextChoices}
                  end,
                  unique_choices(N, Choices)).

%% @doc
%% Create an iterator that returns all combinations of elements from
%% `Choices' that are `N' elements long. If the `repetition' property
%% is passed in `Options', combinations with repeated elements of
%% `Choices' are included.
%%
%% If the elements of `Choices' are sorted, the resulting combinations
%% will also be sorted.
%% @end
-spec combinations(N, Choices, Options) -> Iterator when
      N :: non_neg_integer(),
      Choices :: [Elem],
      Options :: permutation_options(),
      Iterator :: llists:iterator([Elem]).
combinations(N, Choices, Options) when is_list(Options) ->
    Repetitions = proplists:get_bool('repetitions', Options),
    case Repetitions of
        true ->
            combinations_with_repetitions(N, Choices);
        false ->
            combinations(N, Choices)
    end.

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
                          group_loop(Length, [], FoldIterator)
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
                          groupwith_loop(Pred, [], FoldIterator)
                  end,
                  Iterator).

%% @see permutations/3
-spec permutations(N, Choices) -> Iterator when
      N :: non_neg_integer(),
      Choices :: [Elem],
      Iterator :: llists:iterator([Elem]).
permutations(N, Choices) when N >= 0, is_list(Choices) ->
    llists:unfold(fun (none) ->
                          none;
                      (Zippers) ->
                          NextChoice = zipper_choice(Zippers),
                          NextZippers = next_permutation(Zippers),
                          {NextChoice, NextZippers}
                  end,
                  zipper_choices(N, Choices)).

%% @doc
%% Create an iterator that returns all permutations of elements from
%% `Choices' that are `N' elements long. If the `repetition' property
%% is passed in `Options', permutations with repeated elements of
%% `Choices' are included.
%%
%% If the elements of `Choices' are sorted, the resulting permutations
%% will also be sorted.
%% @end
-spec permutations(N, Choices, Options) -> Iterator when
      N :: non_neg_integer(),
      Choices :: [Elem],
      Options :: permutation_options(),
      Iterator :: llists:iterator([Elem]).
permutations(N, Choices, Options) when is_list(Options) ->
    Repetitions = proplists:get_bool('repetitions', Options),
    case Repetitions of
        true ->
            permutations_with_repetitions(N, Choices);
        false ->
            permutations(N, Choices)
    end.

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

%% TODO: Uniform random number stream.
%% TODO: Random choice from list stream.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

repeated_choices(N, Choices) when N >= 0 ->
    lists:duplicate(N, Choices).

unique_choices(N, Choices) ->
    unique_choices(N, Choices, []).

unique_choices(0, _Choices, Acc) ->
    Acc;
unique_choices(_N, [], _Acc) ->
    none;
unique_choices(N, [_ | Tail] = Choices, Acc) when N > 0 ->
    unique_choices(N - 1, Tail, [Choices | Acc]).

next_choice(Choices) ->
    lists:foldl(
      fun ([Head | _], Acc) -> [Head | Acc] end,
      [],
      Choices).

next_combination(Choices) ->
    next_combination(1, Choices).

next_combination(_N, []) ->
    none;
next_combination(N, [Current | Choices]) when N >= length(Current) ->
    next_combination(N + 1, Choices);
next_combination(N, [[_ | Tail] | Choices]) ->
    unique_choices(N, Tail) ++ Choices.

next_rep_combination(Choices) ->
    next_rep_combination(1, Choices).

next_rep_combination(_N, []) ->
    none;
next_rep_combination(N, [[_] | Choices]) ->
    next_rep_combination(N + 1, Choices);
next_rep_combination(N, [[_ | Tail] | Choices]) ->
    repeated_choices(N, Tail) ++ Choices.

combinations_with_repetitions(N, Choices) when N >= 0, is_list(Choices) ->
    llists:unfold(fun (none) ->
                          none;
                      (CurrentChoices) ->
                          NextChoice = next_choice(CurrentChoices),
                          NextChoices = next_rep_combination(CurrentChoices),
                          {NextChoice, NextChoices}
                  end,
                  repeated_choices(N, Choices)).

zipper_choices(N, Choices) ->
    zipper_choices(N, Choices, []).

zipper_choices(0, _Choices, Acc) ->
    Acc;
zipper_choices(_N, [], _Acc) ->
    none;
zipper_choices(N, [Head | Tail], Acc) ->
    zipper_choices(N - 1, Tail, [#zipper{heads=[Head], tail=Tail} | Acc]).

zipper_choice(Zippers) ->
    lists:foldl(
      fun (#zipper{heads=[Head | _]}, Acc) -> [Head | Acc] end,
      [],
      Zippers).

next_permutation(Zippers) ->
    next_permutation(1, Zippers).

next_permutation(_N, []) ->
    none;
next_permutation(N, [#zipper{tail=[]} | Zippers]) ->
    next_permutation(N + 1, Zippers);
next_permutation(N, [#zipper{heads=Heads, tail=[Head | Tail]} | Zippers]) ->
    zipper_choices(N - 1, lists:reverse(Heads) ++ Tail) ++
    [#zipper{heads=[Head | Heads], tail=Tail}] ++
    Zippers.

next_rep_permutation(Original, Choices) ->
    next_rep_permutation(1, Original, Choices).

next_rep_permutation(_N, _Original, []) ->
    none;
next_rep_permutation(N, Original, [[_] | Choices]) ->
    next_rep_permutation(N + 1, Original, Choices);
next_rep_permutation(N, Original, [[_ | Tail] | Choices]) ->
    repeated_choices(N - 1, Original) ++ [Tail] ++ Choices.

permutations_with_repetitions(N, Choices) when N >= 0, is_list(Choices) ->
    llists:unfold(fun (none) ->
                          none;
                      (CurrentChoices) ->
                          NextChoice = next_choice(CurrentChoices),
                          NextChoices = next_rep_permutation(Choices, CurrentChoices),
                          {NextChoice, NextChoices}
                  end,
                  repeated_choices(N, Choices)).

group_loop(_N, _Acc, none) ->
    none;
group_loop(0, Acc, Iterator) ->
    {lists:reverse(Acc), Iterator};
group_loop(N, Acc, Iterator) when N > 0 ->
    case {Acc, llists:next(Iterator)} of
        {[], []} ->
            none;
        {_, []} ->
            {lists:reverse(Acc), none};
        {_, [Elem | NextIterator]} ->
            group_loop(N - 1, [Elem | Acc], NextIterator)
    end.

groupwith_loop(_Pred, _Acc, none) ->
    none;
groupwith_loop(Pred, Acc, Iterator) ->
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
                    groupwith_loop(Pred, [Elem | Acc], NextIterator)
            end
    end.
