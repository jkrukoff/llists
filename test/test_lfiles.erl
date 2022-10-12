%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for src/lfiles.erl
%%% @end
%%%-------------------------------------------------------------------
-module(test_lfiles).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

read_test() ->
    {ok, File} = file:open("test/data/alpha.txt", [read]),
    ?assertEqual(
        ["A", "\n", "B", "\n", "C", "\n"],
        llists:to_list(
            lfiles:read(File, 1)
        )
    ).

get_chars_test() ->
    {ok, File} = file:open("test/data/alpha.txt", [read]),
    ?assertEqual(
        ["A", "\n", "B", "\n", "C", "\n"],
        llists:to_list(
            lfiles:get_chars(File, undefined, 1)
        )
    ).

read_line_test() ->
    {ok, File} = file:open("test/data/alpha.txt", [read]),
    ?assertEqual(
        ["A\n", "B\n", "C\n"],
        llists:to_list(
            lfiles:read_line(File)
        )
    ).

get_line_test() ->
    {ok, File} = file:open("test/data/alpha.txt", [read]),
    ?assertEqual(
        ["A\n", "B\n", "C\n"],
        llists:to_list(
            lfiles:get_line(File, undefined)
        )
    ).

write_test() ->
    ?assertEqual(
        "12345",
        with_temp_file(fun(File) ->
            lfiles:write(File, llists:from_list(["1", "2", "3", "4", "5"]))
        end)
    ).

put_chars_test() ->
    ?assertEqual(
        "12345",
        with_temp_file(fun(File) ->
            lfiles:put_chars(File, llists:from_list(["1", "2", "3", "4", "5"]))
        end)
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

with_temp_file(Test) ->
    Suffix = "test-lfiles-" ++ integer_to_list(binary:decode_unsigned(rand:bytes(8)), 36),
    TempPath = filename:basedir(user_cache, "llists"),
    TempFilename = filename:join(TempPath, Suffix),
    ok = filelib:ensure_dir(TempFilename),
    {ok, File} = file:open(TempFilename, [read, write]),
    try
        ok = file:write(File, <<>>),
        ?assertEqual(ok, Test(File)),
        read_temp_file(File)
    of
        TestResult -> TestResult
    after
        ok = file:close(File),
        ok = file:delete(TempFilename)
    end.

read_temp_file(File) ->
    {ok, _} = file:position(File, bof),
    read_temp_file(File, []).

read_temp_file(File, Acc) ->
    case io:get_chars(File, undefined, 1024) of
        eof ->
            unicode:characters_to_list(lists:reverse(Acc));
        {error, _Reason} = Error ->
            throw(Error);
        Data ->
            read_temp_file(File, [Data | Acc])
    end.
