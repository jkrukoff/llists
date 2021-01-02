%%%-------------------------------------------------------------------
%%% @doc
%%% Property tests for src/llists_utils.erl
%%% @end
%%%-------------------------------------------------------------------
-module(prop_llists_utils).

-define(MAX_CYCLE, 3).

-include_lib("proper/include/proper.hrl").

%% Property options.
-export([]).

%%%===================================================================
%%% Tests
%%%===================================================================

prop_choice() ->
    ?FORALL(
        {N, List},
        {pos_integer(), non_empty(list())},
        begin
            Choices = llists:to_list(
                llists:sublist(
                    llists_utils:choice(List),
                    N
                )
            ),
            lists:all(
                fun(Choice) -> lists:member(Choice, List) end,
                Choices
            )
        end
    ).

prop_combinations() ->
    ?FORALL(
        {N, List},
        ?LET(List, small_list(), {integer(0, length(List)), List}),
        llists:to_list(
            llists_utils:combinations(N, List)
        ) ==
            combinations(N, List)
    ).

prop_combinations_with_repetitions() ->
    ?FORALL(
        {N, List},
        ?SUCHTHAT(
            {N, List},
            ?LET(List, small_list(), {integer(0, length(List) + 1), List}),
            not (N == 1 andalso length(List) == 0)
        ),
        llists:to_list(
            llists_utils:combinations(N, List, [repetitions])
        ) ==
            rep_combinations(N, List)
    ).

prop_cycle() ->
    ?FORALL(
        {Length, List},
        ?LET(
            List,
            non_empty(list()),
            {integer(1, length(List) * ?MAX_CYCLE), List}
        ),
        llists:to_list(
            llists:sublist(
                llists_utils:cycle(
                    llists:from_list(List)
                ),
                Length
            )
        ) ==
            lists:sublist(
                lists:append(lists:duplicate(?MAX_CYCLE, List)),
                Length
            )
    ).

prop_enumerate() ->
    ?FORALL(
        List,
        list(),
        llists:to_list(
            llists_utils:enumerate(
                llists:from_list(List)
            )
        ) == enumerate(List)
    ).

prop_group() ->
    ?FORALL(
        {Length, List},
        ?LET(
            List,
            list(),
            {integer(1, length(List) + 1), List}
        ),
        begin
            Groups = llists:to_list(
                llists_utils:group(
                    Length,
                    llists:from_list(List)
                )
            ),
            Lengths = [length(G) || G <- Groups],
            % Groups should all be less than or equal to expected
            % length.
            IsExpectedLength = lists:all(
                fun(L) -> L =< Length end,
                Lengths
            ),
            % Groups should never be empty.
            IsNotEmpty = lists:all(fun(L) -> L /= 0 end, Lengths),
            % All elements should be present.
            IsSame = lists:append(Groups) == List,
            IsExpectedLength and IsNotEmpty and IsSame
        end
    ).

prop_groupwith() ->
    ?FORALL(
        {Pred, List},
        {function(1, boolean()), list()},
        begin
            Groups = llists:to_list(
                llists_utils:groupwith(
                    Pred,
                    llists:from_list(List)
                )
            ),
            Lengths = [length(G) || G <- Groups],
            % Groups should never be empty.
            IsNotEmpty = lists:all(fun(L) -> L /= 0 end, Lengths),
            % All elements should be present.
            IsSame = lists:append(Groups) == List,
            IsNotEmpty and IsSame
        end
    ).

prop_permutations() ->
    ?FORALL(
        {N, List},
        ?LET(List, small_list(), {integer(0, length(List)), List}),
        llists:to_list(
            llists_utils:permutations(N, List)
        ) ==
            permutations(N, List)
    ).

prop_permutations_with_repetitions() ->
    ?FORALL(
        {N, List},
        ?SUCHTHAT(
            {N, List},
            ?LET(List, small_list(), {integer(0, length(List) + 1), List}),
            not (N == 1 andalso length(List) == 0)
        ),
        llists:to_list(
            llists_utils:permutations(N, List, [repetitions])
        ) ==
            rep_permutations(N, List)
    ).

prop_random_0() ->
    ?FORALL(
        Length,
        pos_integer(),
        begin
            Values = llists:to_list(
                llists:sublist(
                    llists_utils:random(),
                    Length
                )
            ),
            IsFloat = lists:all(fun is_float/1, Values),
            InRange = lists:all(
                fun(X) -> X >= 0.0 andalso X < 1.0 end,
                Values
            ),
            IsFloat and InRange
        end
    ).

prop_random_1() ->
    ?FORALL(
        {N, Length},
        {pos_integer(), pos_integer()},
        begin
            Values = llists:to_list(
                llists:sublist(
                    llists_utils:random(N),
                    Length
                )
            ),
            IsInteger = lists:all(fun is_integer/1, Values),
            InRange = lists:all(
                fun(X) -> X >= 1 andalso X =< N end,
                Values
            ),
            IsInteger and InRange
        end
    ).

prop_unique_1() ->
    ?FORALL(
        List,
        duplicates(),
        llists:to_list(
            llists_utils:unique(
                llists:from_list(List)
            )
        ) ==
            lists:usort(List)
    ).

prop_unique_2() ->
    Compare = fun(A, B) -> A == B end,
    ?FORALL(
        List,
        duplicates(),
        llists:to_list(
            llists_utils:unique(
                Compare,
                llists:from_list(List)
            )
        ) ==
            lists:usort(List)
    ).

%%%===================================================================
%%% Generators
%%%===================================================================

small_list() ->
    small_list(any()).

small_list(Of) ->
    ?SIZED(
        Size,
        if
            Size > 6 ->
                resize(6, list(Of));
            Size =< 6 ->
                list(Of)
        end
    ).

duplicates() ->
    frequency([
        {1, ?LET(List, list(), lists:sort(List ++ List))},
        {1, ?LET(List, list(), lists:sort(List))}
    ]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

enumerate(List) when is_list(List) ->
    lists:zip(lists:seq(1, length(List)), List).

remove(N, List) when N >= 0, is_list(List) ->
    {Before, [_ | After]} = lists:split(N - 1, List),
    Before ++ After.

combinations(0, _) ->
    [[]];
combinations(_, []) ->
    [];
combinations(N, [Head | Tail]) ->
    [
        [Head | Choices]
        || Choices <- combinations(N - 1, Tail)
    ] ++ combinations(N, Tail).

rep_combinations(0, _) ->
    [[]];
rep_combinations(_, []) ->
    [];
rep_combinations(N, [Head | Tail] = All) ->
    [
        [Head | Choices]
        || Choices <- rep_combinations(N - 1, All)
    ] ++ rep_combinations(N, Tail).

permutations(0, _) ->
    [[]];
permutations(_, []) ->
    [[]];
permutations(N, All) ->
    [
        [Head | Tail]
        || {Index, Head} <- enumerate(All),
           Tail <- permutations(N - 1, remove(Index, All))
    ].

rep_permutations(0, _) ->
    [[]];
rep_permutations(_, []) ->
    [[]];
rep_permutations(N, All) ->
    [
        [Head | Tail]
        || Head <- All,
           Tail <- rep_permutations(N - 1, All)
    ].
