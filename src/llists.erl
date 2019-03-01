%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llists).

-record(llist, {next, acc}).

-type accumulator() :: any().

-type iterator() :: iterator(any()).
-type iterator(Over) :: fun((accumulator()) ->
                            {Elem :: Over, Acc :: accumulator()} | none.

-type llist() :: llist(any()).
-type llist(Over) :: #llist{next :: iterator(Over)),
                            acc :: accumulator()}.

-type lazy_list() :: lazy_list(any()).
-type lazy_list(Over) :: nonempty_improper_list(Over, llist(Over)) | [].

%% API
-export([is_llist/1,
         unfold/2,
         next/1,
         hd/1,
         tl/1,
         % Lazy lists.
         from_list/1,
         range/2,
         range/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_llist(llist()) -> true;
              (any()) -> false.
is_llist(#llist{}) ->
    true;
is_llist(_) ->
    false.

-spec unfold(Fun :: iterator(), Acc :: accumulator()) -> llist().
unfold(Fun, Acc) ->
    new(Fun, Acc).

-spec next(llist()) -> lazy_list().
next(#llist{next=Next, acc=Acc}) ->
    case Fun(Acc) of
        {Elem, NewAcc} ->
            [Elem | new(Fun, NewAcc)];
        none ->
            []
    end.

-spec hd(llist()) -> any().
hd(#llist{} = LList) ->
    erlang:hd(next(LList)).

-spec tl(llist()) -> llist().
tl(#llist{} = LList) ->
    erlang:tl(next(LList)).

%%%===================================================================
%%% API - Lazy Lists
%%%===================================================================

from_list(List) ->
    new(fun ([]) -> none;
            ([Head | Tail]) -> {Head, Tail}
        end,
        List).

range(Start, Stop) when Start =< Stop ->
    range(Start, Stop, 1);
range(Start, Stop) when Start > Stop ->
    range(Start, Stop, -1).

range(Start, Stop, Step) when Start =< Stop ->
    new(fun (Acc) when Acc < Stop -> {Acc, Acc + Step};
            (Acc) when Acc >= Stop -> none
        end,
        Start);
range(Start, Stop, Step) when Start >= Stop ->
    new(fun (Acc) when Acc > Stop -> {Acc, Acc + Step};
            (Acc) when Acc =< Stop -> none
        end,
        Start).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

new(Fun, Acc) ->
    #llist{next=Fun, acc=Acc}.

compose(Fun, #llist{next=Next} = LList) ->
    LList#llist{next=fun(Acc) ->
                             case Next(Acc) of
                                 {Elem, NewAcc} ->
                                     {Fun(Elem), NewAcc};
                                 none ->
                                     none
                             end
                     end}.
