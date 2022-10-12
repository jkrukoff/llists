%%%-------------------------------------------------------------------
%%% @doc
%%% A lazily evaluated file module. This module provides replicas of
%%% functions from the kernel `file' and stdlib `io' modules designed
%%% to work with iterators as defined by the `llists' module.
%%%
%%% All iterators created by this module work by side effect, making
%%% them impure. As such, they should only be evaluated once.
%%%
%%% As there is no guarantee that an iterator will be completely
%%% evaluated, this module expects the lifecycle of the opened file
%%% process to be managed by the caller.
%%% @end
%%%-------------------------------------------------------------------
-module(lfiles).

%% API

-export([
    % Iterator construction.
    read/2,
    get_chars/3,
    read_line/1,
    get_line/2,
    % Iterator evaluation.
    write/2,
    put_chars/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% @end
-spec read(IODevice, Number) -> Iterator when
    IODevice :: file:io_device(),
    Number :: non_neg_integer(),
    Iterator :: llists:iterator(Data),
    Data :: string() | binary().
read(IODevice, Number) ->
    file_read_iterator(fun() -> file:read(IODevice, Number) end).

-spec get_chars(IODevice, Prompt, Number) -> Iterator when
    IODevice :: file:io_device(),
    Prompt :: io:prompt(),
    Number :: non_neg_integer(),
    Iterator :: llists:iterator(Data),
    Data :: string() | unicode:unicode_binary().
get_chars(IODevice, Prompt, Number) ->
    io_read_iterator(fun() -> io:get_chars(IODevice, Prompt, Number) end).

-spec read_line(IODevice) -> Iterator when
    IODevice :: file:io_device(),
    Iterator :: llists:iterator(Data),
    Data :: string() | binary().
read_line(IODevice) ->
    file_read_iterator(fun() -> file:read_line(IODevice) end).

-spec get_line(IODevice, Prompt) -> Iterator when
    IODevice :: file:io_device(),
    Prompt :: io:prompt(),
    Iterator :: llists:iterator(Data),
    Data :: string() | unicode:unicode_binary().
get_line(IODevice, Prompt) ->
    io_read_iterator(fun() -> io:get_line(IODevice, Prompt) end).

%% The iterator will be fully evaluated, infinite iterators will never
%% return.
-spec write(IODevice, Iterator) -> ok | {error, Reason} when
    IODevice :: file:io_device(),
    Iterator :: llists:iterator(file:io_data()),
    Reason :: file:posix() | badarg | terminated.
write(IODevice, Iterator) ->
    true = llists:is_iterator(Iterator),
    write_loop(IODevice, llists:next(Iterator), ok).

%% The iterator will be fully evaluated, infinite iterators will never
%% return.
-spec put_chars(IODevice, Iterator) -> ok when
    IODevice :: file:io_device(),
    Iterator :: llists:iterator(unicode:chardata()).
put_chars(IODevice, Iterator) ->
    true = llists:is_iterator(Iterator),
    llists:foreach(fun(CharData) -> ok = io:put_chars(IODevice, CharData) end, Iterator).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

% Iterators don't have a way to propagate failure, so we'll throw an
% exception if the underlying read reports an error.
file_read_iterator(Read) ->
    llists:unfold(
        fun(undefined) ->
            case Read() of
                {ok, Data} ->
                    {Data, undefined};
                eof ->
                    none;
                {error, Reason} ->
                    throw({file_read_error, Reason})
            end
        end,
        undefined
    ).

% Iterators don't have a way to propagate failure, so we'll throw an
% exception if the underlying read reports an error.
io_read_iterator(Read) ->
    llists:unfold(
        fun(undefined) ->
            case Read() of
                eof ->
                    none;
                {error, Reason} ->
                    throw({io_read_error, Reason});
                Data ->
                    {Data, undefined}
            end
        end,
        undefined
    ).

write_loop(_IODevice, _Iterator, {error, Reason}) ->
    {error, Reason};
write_loop(_IODevice, [], ok) ->
    ok;
write_loop(IODevice, [Bytes | Iterator], ok) ->
    write_loop(IODevice, llists:next(Iterator), file:write(IODevice, Bytes)).
