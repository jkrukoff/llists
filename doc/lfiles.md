# Module lfiles

- [Description](#description)
- [Function Index](#index)
- [Function Details](#functions)

A lazily evaluated file module.

<a name="description"></a>

## Description

This module provides replicas of
functions from the kernel `file` and stdlib `io` modules designed
to work with iterators as defined by the `llists` module.

All iterators created by this module work by side effect, making
them impure. As such, they should only be evaluated once.

As there is no guarantee that an iterator will be completely
evaluated, this module expects the lifecycle of the opened file
process to be managed by the caller.<a name="index"></a>

## Function Index

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_chars-3">get_chars/3</a></td><td>
Create an iterator that returns chunks of <code>Number</code> characters from
<code>IODevice</code>, prompting each read with <code>Prompt</code>.</td></tr><tr><td valign="top"><a href="#get_line-2">get_line/2</a></td><td>
Create an iterator that returns lines of data from a file
referenced by <code>IODevice</code>, prompting each read with <code>Prompt</code>.</td></tr><tr><td valign="top"><a href="#put_chars-2">put_chars/2</a></td><td>
Fully evaluate <code>Iterator</code> and write the characters returned to the
file referenced by <code>IODevice</code>.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>
Create an iterator that returns chunks of data from a file
referenced by <code>IODevice</code> of approximately <code>Number</code>
bytes/characters.</td></tr><tr><td valign="top"><a href="#read_line-1">read_line/1</a></td><td>
Create an iterator that returns lines of data from a file
referenced by <code>IODevice</code>.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>
Fully evaluate <code>Iterator</code> and write the bytes returned to the file
referenced by <code>IODevice</code>.</td></tr></table>

<a name="functions"></a>

## Function Details

<a name="get_chars-3"></a>

### get_chars/3

<pre><code>
get_chars(IODevice, Prompt, Number) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IODevice = <a href="/home/john/Local/kernel/doc/file.md#type-io_device">file:io_device()</a></code></li><li><code>Prompt = <a href="/home/john/Local/stdlib/doc/io.md#type-prompt">io:prompt()</a></code></li><li><code>Number = non_neg_integer()</code></li><li><code>Iterator = <a href="/home/john/Local/llists/doc/llists.md#type-iterator">llists:iterator</a>(Data)</code></li><li><code>Data = string() | <a href="/home/john/Local/stdlib/doc/unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li></ul>

Create an iterator that returns chunks of `Number` characters from
`IODevice`, prompting each read with `Prompt`.

If the get_chars call fails, an error of form
`{io_read_error, Reason}` will be thrown by the iterator.

**See also:** [io:get_chars/3](io.md#get_chars-3).

<a name="get_line-2"></a>

### get_line/2

<pre><code>
get_line(IODevice, Prompt) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IODevice = <a href="/home/john/Local/kernel/doc/file.md#type-io_device">file:io_device()</a></code></li><li><code>Prompt = <a href="/home/john/Local/stdlib/doc/io.md#type-prompt">io:prompt()</a></code></li><li><code>Iterator = <a href="/home/john/Local/llists/doc/llists.md#type-iterator">llists:iterator</a>(Data)</code></li><li><code>Data = string() | <a href="/home/john/Local/stdlib/doc/unicode.md#type-unicode_binary">unicode:unicode_binary()</a></code></li></ul>

Create an iterator that returns lines of data from a file
referenced by `IODevice`, prompting each read with `Prompt`.

The trailing linefeed (`\n`) character is returned as part of the
line.

If the get_line call fails, an error of form
`{io_read_error, Reason}` will be thrown by the iterator.

**See also:** [io:get_line/2](io.md#get_line-2).

<a name="put_chars-2"></a>

### put_chars/2

<pre><code>
put_chars(IODevice, Iterator) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>IODevice = <a href="/home/john/Local/kernel/doc/file.md#type-io_device">file:io_device()</a></code></li><li><code>Iterator = <a href="/home/john/Local/llists/doc/llists.md#type-iterator">llists:iterator</a>(<a href="/home/john/Local/stdlib/doc/unicode.md#type-chardata">unicode:chardata()</a>)</code></li></ul>

Fully evaluate `Iterator` and write the characters returned to the
file referenced by `IODevice`.

The iterator will be fully evaluated, infinite iterators will never
return (or will fill up the disk and error!).

**See also:** [io:put_chars/2](io.md#put_chars-2).

<a name="read-2"></a>

### read/2

<pre><code>
read(IODevice, Number) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IODevice = <a href="/home/john/Local/kernel/doc/file.md#type-io_device">file:io_device()</a></code></li><li><code>Number = non_neg_integer()</code></li><li><code>Iterator = <a href="/home/john/Local/llists/doc/llists.md#type-iterator">llists:iterator</a>(Data)</code></li><li><code>Data = string() | binary()</code></li></ul>

Create an iterator that returns chunks of data from a file
referenced by `IODevice` of approximately `Number`
bytes/characters.

If the read fails, an error of form `{file_read_error, Reason}`
will be thrown by the iterator.

**See also:** [file:read/2](file.md#read-2).

<a name="read_line-1"></a>

### read_line/1

<pre><code>
read_line(IODevice) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IODevice = <a href="/home/john/Local/kernel/doc/file.md#type-io_device">file:io_device()</a></code></li><li><code>Iterator = <a href="/home/john/Local/llists/doc/llists.md#type-iterator">llists:iterator</a>(Data)</code></li><li><code>Data = string() | binary()</code></li></ul>

Create an iterator that returns lines of data from a file
referenced by `IODevice`.

The trailing linefeed (`\n`) character is returned as part of the
line.

If the read fails, an error of form `{file_read_error, Reason}`
will be thrown by the iterator.

**See also:** [file:read_line/1](file.md#read_line-1).

<a name="write-2"></a>

### write/2

<pre><code>
write(IODevice, Iterator) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>IODevice = <a href="/home/john/Local/kernel/doc/file.md#type-io_device">file:io_device()</a></code></li><li><code>Iterator = <a href="/home/john/Local/llists/doc/llists.md#type-iterator">llists:iterator</a>(<a href="/home/john/Local/kernel/doc/file.md#type-io_data">file:io_data()</a>)</code></li><li><code>Reason = <a href="/home/john/Local/kernel/doc/file.md#type-posix">file:posix()</a> | badarg | terminated</code></li></ul>

Fully evaluate `Iterator` and write the bytes returned to the file
referenced by `IODevice`.

`ok` is returned on success, but if the write fails an error of
form `{error, Reason}` will be returned.

The iterator will be fully evaluated, infinite iterators will never
return (or will fill up the disk and error!).

**See also:** [file:write/2](file.md#write-2).
