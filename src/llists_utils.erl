%%%-------------------------------------------------------------------
%%% @doc
%%% Additional iterator utilities that are not replicas of `lists'
%%% module functionality. These functions are kept separate to avoid
%%% any future name clashes with additions to the stdlib.
%%%
%%% Unlike the functions in `llists', these utility functions do not
%%% follow the same strict transformation rules. Instead, inputs and
%%% outputs generally follow evaluation needs with eagerly evaluated
%%% values passed as lists and lazily evaluated ones passed as
%%% iterators.
%%% @end
%%%-------------------------------------------------------------------
-module(llists_utils).

-record(zipper, {heads, tail}).

-type permutation_options() :: proplists:proplist().

%% API
-export([
    choice/1,
    combinations/2,
    combinations/3,
    cycle/1,
    enumerate/1,
    group/2,
    groupwith/2,
    permutations/2,
    permutations/3,
    random/0,
    random/1,
    unique/1,
    unique/2
]).

-export_type([permutation_options/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Create an infinite iterator that returns random elements from the
%% given list of `Choices'. Each iterator returns a unique sequence
%% and returns the same unique sequence each time it is evaluated.
%% @end
-spec choice(Choices) -> Iterator when
    Choices :: [Elem, ...],
    Iterator :: llists:iterator(Elem).
choice(Choices) when length(Choices) > 0 ->
    Length = length(Choices),
    Enumerated = lists:zip(lists:seq(1, Length), Choices),
    Lookup = maps:from_list(Enumerated),
    llists:map(
        fun(I) -> maps:get(I, Lookup) end,
        random(Length)
    ).

%% @see combinations/3
-spec combinations(N, Choices) -> Iterator when
    N :: non_neg_integer(),
    Choices :: [Elem],
    Iterator :: llists:iterator([Elem]).
combinations(N, Choices) when N >= 0, is_list(Choices) ->
    llists:unfold(
        fun
            (none) ->
                none;
            (CurrentChoices) ->
                NextChoice = next_choice(CurrentChoices),
                NextChoices = next_combination(CurrentChoices),
                {NextChoice, NextChoices}
        end,
        unique_choices(N, Choices)
    ).

%% @doc
%% Create an iterator that returns all combinations of elements from
%% `Choices' that are `N' elements long. If the `repetitions' property
%% is passed in `Options', combinations with repeated elements of
%% `Choices' are included.
%%
%% Examples:
%% ```
%% > llists:to_list(
%%      llists_utils:combinations(2, [1, 2, 3]).
%% [[1,2],[1,3],[2,3]]
%% > llists:to_list(
%%      llists_utils:combinations(2, [1, 2, 3], [repetitions]).
%% [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
%% '''
%%
%% If the elements of `Choices' are sorted, the order of the resulting
%% combinations will also be sorted.
%% @end
-spec combinations(N, Choices, Options) -> Iterator when
    N :: non_neg_integer(),
    Choices :: [Elem],
    Options :: permutation_options(),
    Iterator :: llists:iterator([Elem]).
combinations(N, Choices, Options) when is_list(Options) ->
    Repetitions = proplists:get_bool(repetitions, Options),
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
%% Given an existing `Iterator1' creates a new `Iterator2' which
%% returns each element of the original iterator as a tuple of the
%% number of elements returned and the element itself.
%%
%% Example:
%% ```
%% > llists:to_list(
%%      llists_utils:enumerate(
%%          llits:from_list([one, two, three]))).
%% [{1,one},{2,two},{3,three}]
%% '''
%% @end
-spec enumerate(Iterator1) -> Iterator2 when
    Iterator1 :: llists:iterator(Elem),
    Iterator2 :: llists:iterator({Index, Elem}),
    Index :: pos_integer().
enumerate(Iterator) ->
    true = llists:is_iterator(Iterator),
    llists:unfold(
        fun({I, FoldIterator}) ->
            case llists:next(FoldIterator) of
                [] ->
                    none;
                [Elem | Next] ->
                    {{I, Elem}, {I + 1, Next}}
            end
        end,
        {1, Iterator}
    ).

%% @doc
%% Create an iterator that returns groups of elements from `Iterator1'
%% as a list of at least `Length' elements.
%%
%% Example:
%% ```
%% > llists:to_list(
%%      llists_utils:group(
%%          2,
%%          llists:from_list([1, 2, 3, 4, 5]))).
%% [[1,2],[3,4],[5]]
%% '''
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
    llists:unfold(
        fun(FoldIterator) ->
            group_loop(Length, [], FoldIterator)
        end,
        Iterator
    ).

%% @doc
%% Create an iterator that returns groups of elements from `Iterator1'
%% based on the return value of `Pred(Elem)'. If the predicate
%% function returns `true' it signals the end of a group which will be
%% returned as a list. If the predicate returns `false', the element
%% will be included in the next group returned. Even if the predicate
%% function returns `false' for the last element, the final group will
%% still be returned.
%%
%% Example:
%% ```
%% > llists:to_list(
%%      llists_utils:groupwith(
%%          fun (Elem) -> Elem rem 2 == 0 end,
%%          llists:from_list([1, 2, 3, 4, 5]))).
%% [[1,2],[3,4],[5]]
%% '''
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
    llists:unfold(
        fun(FoldIterator) ->
            groupwith_loop(Pred, [], FoldIterator)
        end,
        Iterator
    ).

%% @see permutations/3
-spec permutations(N, Choices) -> Iterator when
    N :: non_neg_integer(),
    Choices :: [Elem],
    Iterator :: llists:iterator([Elem]).
permutations(N, Choices) when N >= 0, is_list(Choices) ->
    llists:unfold(
        fun
            (none) ->
                none;
            (Zippers) ->
                NextChoice = zipper_choice(Zippers),
                NextZippers = next_permutation(Zippers),
                {NextChoice, NextZippers}
        end,
        zipper_choices(N, Choices)
    ).

%% @doc
%% Create an iterator that returns all permutations of elements from
%% `Choices' that are `N' elements long. If the `repetitions' property
%% is passed in `Options', permutations with repeated elements of
%% `Choices' are included.
%%
%% Examples:
%% ```
%% > llists:to_list(
%%      llists_utils:permutations(2, [1, 2, 3]).
%% [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
%% > llists:to_list(
%%      llists_utils:permutations(2, [1, 2, 3], [repetitions]).
%% [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
%% '''
%%
%% If the elements of `Choices' are sorted, the order of the resulting
%% permutations will also be sorted.
%% @end
-spec permutations(N, Choices, Options) -> Iterator when
    N :: non_neg_integer(),
    Choices :: [Elem],
    Options :: permutation_options(),
    Iterator :: llists:iterator([Elem]).
permutations(N, Choices, Options) when is_list(Options) ->
    Repetitions = proplists:get_bool(repetitions, Options),
    case Repetitions of
        true ->
            permutations_with_repetitions(N, Choices);
        false ->
            permutations(N, Choices)
    end.

%% @doc
%% Create an infinite iterator that returns random floats in the range
%% `[0.0, 1.0)'. Each iterator returns a unique sequence and returns
%% the same unique sequence each time it is evaluated.
%% @end
%% @see rand:uniform/0
-spec random() -> Iterator when Iterator :: llists:iterator(float()).
random() ->
    llists:unfold(
        fun(Seed) -> rand:uniform_s(Seed) end,
        rand:seed_s(exrop)
    ).

%% @doc
%% Create an infinite iterator that returns random integers in the range
%% `[1, N)'. Each iterator returns a unique sequence and returns
%% the same unique sequence each time it is evaluated.
%% @end
%% @see rand:uniform/1
-spec random(N) -> Iterator when
    N :: pos_integer(),
    Iterator :: llists:iterator(float()).
random(N) when N >= 1 ->
    llists:unfold(
        fun(Seed) -> rand:uniform_s(N, Seed) end,
        rand:seed_s(exrop)
    ).

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
%% Example:
%% ```
%% > llists:to_list(
%%      llists_utils:unique(
%%          llists:from_list([1, 1, 2, 2, 1, 1]))).
%% [1,2,1]
%% '''
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
    llists:unfold(
        fun
            Next({_Prev, []}) ->
                none;
            Next({Prev, FoldIterator}) ->
                case {Prev, llists:next(FoldIterator)} of
                    {_Prev, []} ->
                        none;
                    {first, [Elem | NextIterator]} ->
                        {Elem, {{previous, Elem}, NextIterator}};
                    {{previous, PrevElem} = Prev, [Elem | NextIterator]} ->
                        case Fun(Elem, PrevElem) of
                            true ->
                                Next({Prev, NextIterator});
                            false ->
                                {Elem, {{previous, Elem}, NextIterator}}
                        end
                end
        end,
        {first, Iterator}
    ).

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
        fun([Head | _], Acc) -> [Head | Acc] end,
        [],
        Choices
    ).

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
    llists:unfold(
        fun
            (none) ->
                none;
            (CurrentChoices) ->
                NextChoice = next_choice(CurrentChoices),
                NextChoices = next_rep_combination(CurrentChoices),
                {NextChoice, NextChoices}
        end,
        repeated_choices(N, Choices)
    ).

zipper_choices(N, Choices) ->
    zipper_choices(N, Choices, []).

zipper_choices(0, _Choices, Acc) ->
    Acc;
zipper_choices(_N, [], _Acc) ->
    none;
zipper_choices(N, [Head | Tail], Acc) ->
    zipper_choices(N - 1, Tail, [#zipper{heads = [Head], tail = Tail} | Acc]).

zipper_choice(Zippers) ->
    lists:foldl(
        fun(#zipper{heads = [Head | _]}, Acc) -> [Head | Acc] end,
        [],
        Zippers
    ).

next_permutation(Zippers) ->
    next_permutation(1, Zippers).

next_permutation(_N, []) ->
    none;
next_permutation(N, [#zipper{tail = []} | Zippers]) ->
    next_permutation(N + 1, Zippers);
next_permutation(N, [#zipper{heads = Heads, tail = [Head | Tail]} | Zippers]) ->
    zipper_choices(N - 1, lists:reverse(Heads) ++ Tail) ++
        [#zipper{heads = [Head | Heads], tail = Tail}] ++
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
    llists:unfold(
        fun
            (none) ->
                none;
            (CurrentChoices) ->
                NextChoice = next_choice(CurrentChoices),
                NextChoices = next_rep_permutation(Choices, CurrentChoices),
                {NextChoice, NextChoices}
        end,
        repeated_choices(N, Choices)
    ).

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
