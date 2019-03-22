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

prop_cycle() ->
    ?FORALL({Length, List},
            ?LET(List,
                 non_empty(list()),
                 {integer(1, length(List) * ?MAX_CYCLE), List}),
            llists:to_list(
              llists:sublist(
                llists_utils:cycle(
                  llists:from_list(List)),
                Length)) ==
            lists:sublist(
              lists:append(lists:duplicate(?MAX_CYCLE, List)),
              Length)
           ).

prop_group() ->
    ?FORALL({Length, List},
            ?LET(List,
                 list(),
                 {integer(1, length(List) + 1), List}),
            begin
                Groups = llists:to_list(
                           llists_utils:group(
                             Length,
                             llists:from_list(List))),
                Lengths = [length(G) || G <- Groups],
                % Groups should all be less than or equal to expected
                % length.
                IsExpectedLength = lists:all(fun (L) -> L =< Length end,
                                             Lengths),
                % Groups should never be empty.
                IsNotEmpty = lists:all(fun (L) -> L /= 0 end, Lengths),
                % All elements should be present.
                IsSame = lists:append(Groups) == List,
                IsExpectedLength and IsNotEmpty and IsSame
            end).

prop_groupwith() ->
    ?FORALL({Pred, List},
            {function(1, boolean()), list()},
            begin
                Groups = llists:to_list(
                           llists_utils:groupwith(
                             Pred,
                             llists:from_list(List))),
                Lengths = [length(G) || G <- Groups],
                % Groups should never be empty.
                IsNotEmpty = lists:all(fun (L) -> L /= 0 end, Lengths),
                % All elements should be present.
                IsSame = lists:append(Groups) == List,
                IsNotEmpty and IsSame
            end).

prop_unique_1() ->
    ?FORALL(List,
            duplicates(),
            llists:to_list(
              llists_utils:unique(
                llists:from_list(List))) ==
            lists:usort(List)).

prop_unique_2() ->
    Compare = fun (A, B) -> A == B end,
    ?FORALL(List,
            duplicates(),
            llists:to_list(
              llists_utils:unique(
                Compare,
                llists:from_list(List))) ==
            lists:usort(List)).

%%%===================================================================
%%% Generators
%%%===================================================================

duplicates() ->
    frequency([{1, ?LET(List, list(), lists:sort(List ++ List))},
               {1, ?LET(List, list(), lists:sort(List))}]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
