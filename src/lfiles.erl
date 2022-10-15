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
%% Create an iterator that returns chunks of data from a file
%% referenced by `IODevice' of approximately `Number'
%% bytes/characters.
%%
%% If the read fails, an error of form `{file_read_error, Reason}'
%% will be thrown by the iterator.
%% @end
%% @see file:read/2
-spec read(IODevice, Number) -> Iterator when
    IODevice :: file:io_device(),
    Number :: non_neg_integer(),
    Iterator :: llists:iterator(Data),
    Data :: string() | binary().
read(IODevice, Number) ->
    file_read_iterator(fun() -> file:read(IODevice, Number) end).

%% @doc
%% Create an iterator that returns chunks of `Number' characters from
%% `IODevice', prompting each read with `Prompt'.
%%
%% If the get_chars call fails, an error of form
%% `{io_read_error, Reason}' will be thrown by the iterator.
%% @end
%% @see io:get_chars/3
-spec get_chars(IODevice, Prompt, Number) -> Iterator when
    IODevice :: file:io_device(),
    Prompt :: io:prompt(),
    Number :: non_neg_integer(),
    Iterator :: llists:iterator(Data),
    Data :: string() | unicode:unicode_binary().
get_chars(IODevice, Prompt, Number) ->
    io_read_iterator(fun() -> io:get_chars(IODevice, Prompt, Number) end).

%% @doc
%% Create an iterator that returns lines of data from a file
%% referenced by `IODevice'.
%%
%% The trailing linefeed (`\n') character is returned as part of the
%% line.
%%
%% If the read fails, an error of form `{file_read_error, Reason}'
%% will be thrown by the iterator.
%% @end
%% @see file:read_line/1
-spec read_line(IODevice) -> Iterator when
    IODevice :: file:io_device(),
    Iterator :: llists:iterator(Data),
    Data :: string() | binary().
read_line(IODevice) ->
    file_read_iterator(fun() -> file:read_line(IODevice) end).

%% @doc
%% Create an iterator that returns lines of data from a file
%% referenced by `IODevice', prompting each read with `Prompt'.
%%
%% The trailing linefeed (`\n') character is returned as part of the
%% line.
%%
%% If the get_line call fails, an error of form
%% `{io_read_error, Reason}' will be thrown by the iterator.
%% @end
%% @see io:get_line/2
-spec get_line(IODevice, Prompt) -> Iterator when
    IODevice :: file:io_device(),
    Prompt :: io:prompt(),
    Iterator :: llists:iterator(Data),
    Data :: string() | unicode:unicode_binary().
get_line(IODevice, Prompt) ->
    io_read_iterator(fun() -> io:get_line(IODevice, Prompt) end).

%% @doc
%% Fully evaluate `Iterator' and write the bytes returned to the file
%% referenced by `IODevice'.
%%
%% `ok' is returned on success, but if the write fails an error of
%% form `{error, Reason}' will be returned.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return (or will fill up the disk and error!).
%% @end
%% @see file:write/2
-spec write(IODevice, Iterator) -> ok | {error, Reason} when
    IODevice :: file:io_device(),
    Iterator :: llists:iterator(file:io_data()),
    Reason :: file:posix() | badarg | terminated.
write(IODevice, Iterator) ->
    true = llists:is_iterator(Iterator),
    write_loop(IODevice, llists:next(Iterator), ok).

%% @doc
%% Fully evaluate `Iterator' and write the characters returned to the
%% file referenced by `IODevice'.
%%
%% The iterator will be fully evaluated, infinite iterators will never
%% return (or will fill up the disk and error!).
%% @end
%% @see io:put_chars/2
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
