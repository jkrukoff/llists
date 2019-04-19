# llists #

![Lazy Construction Worker](doc/lazy.png)


### Overview ###

An Erlang/OTP library for working with iterators; an opaque type that is
evaluated as a lazily evaluated list of elements. This allows for memory
efficient processing of sequential data and easy composition of processing on
such lazy lists.

An interface identical to the stdlib `lists` module is implemented, allowing
for easy conversion between list processing code and lazy list processing
code.


## Modules ##

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/jkrukoff/llists/blob/permutations/doc/llists.md" class="module">llists</a></td></tr>
<tr><td><a href="http://github.com/jkrukoff/llists/blob/permutations/doc/llists_utils.md" class="module">llists_utils</a></td></tr></table>


### Getting Started ###

This library is published to [hex.pm](https://hex.pm) as
[llists](https://hex.pm/packages/llists). If you're using
[rebar3](https://www.rebar3.org/) as your build tool, it can be added as a
dependency to your rebar.config as follows:

```
{deps, [{llists}]}.
```


### Usage ###

To use this library, it is first necessary to create an iterator. This can
either be done from an existing data structure using functions like
`llists:from_list/1` or by expressing the continuation programmatically using
functions like `llists:unfold/2`.

Once an iterator is constructed, it can then be evaluated an element at a time
using `llists:next/1` or by using one of the many utility and evaluation
functions in `llists` or `llists_utils`.

`llists` contains an iterator aware version of every function in the stdlib
`lists` module, with guaranteed identical behaviour for side effect free and
finite iterators.


#### Iterators ####

An iterator is an opaque record created by the `llists` module to represent a
continuation. When evaluated by `llists:next/1` an iterator returns a lazy
list, represented by one of two options:

* `[]`: An empty list, signaling that no more elements are available.
* `[Elem | Iterator]`: An improper list consisting of an element and an
  iterator to continue evaluation.


#### Examples ####

As an example task, let us calculate the mean difference between a list of
integer pairs. These pairs are stored in a
[file](http://github.com/jkrukoff/llists/blob/permutations/doc/example.txt) as
comma separated values, one per line. We can use the `llists` module to both
read lines from the file and calculate the average lazily, thus avoiding
loading the entire file into memory during computation.

First, we need to construct the iterator:

```
> {ok, File} = file:open("doc/example.txt", [read]).
{ok,<0.160.0>}
> I = llists:unfold(fun(File) ->
	case file:read_line(File) of
		{ok, Data} ->
			{Data, File};
		eof ->
			file:close(File),
			none
	end
end, File).
{iterator,#Fun<llists.2.51622540>}
```

Next, a loop to parse the strings and calculate the mean difference:

```
> F = fun Calculate(I, Sum, Count) ->
	case llists:next(I) of
		[] ->
			Sum / Count;
		[Elem | Next] ->
			[A, B] = [list_to_integer(string:trim(Part)) ||
				Part <- string:split(Elem, ",")],
			Calculate(Next, Sum + (A - B), Count + 1)
	end
end.
#Fun<erl_eval.42.128620087>
> F(I, 0, 0).
-0.42
```

We could also make use of the utility functions in `llists` and compose the
same result as follows:

```
> Split = llists:map(fun (Elem) ->
	string:split(Elem, ",")
end, I).
{iterator,#Fun<llists.23.51622540>}
> Integers = llists:map(fun (Parts) ->
	[list_to_integer(string:trim(Part)) || Part <- Parts]
end, Split).
{iterator,#Fun<llists.23.51622540>}
> {Sum, Count} = llists:foldl(fun ([A, B], {Sum, Count}) ->
	{Sum + (A - B), Count + 1}
end, {0, 0}, Integers).
{-42,100}
> Sum / Count.
-0.42
```

In both examples, we read only a single line of the file into memory at a
time.

Notice that we couldn't use `llists:sum/1` and `llists:length/1` here instead
of `llists:foldl/3`, since out input iterator has side effects and can only be
evaluated once.


### Contributing ###

Please fork the repo and submit a PR. Tests are run via:

```
rebar3 as test do eunit, proper
```

Documentation is autogenerated using edown and edoc via:

```
rebar3 as markdown edoc
```

The library has only been tested with Erlang/OTP 21 on Ubuntu Linux and
Windows 10. Reports of success (or failure!) on other versions and operating
systems are appreciated.


### Lineage ###

Streams and lazily evaluated lists are common language constructs and much
prior art exists. Scheme's [SRFI-41](https://srfi.schemers.org/srfi-41/srfi-41.html) served as a
useful design document to base this work on.

Other implementations that were used for reference:

* Elixir's standard library [Stream](https://hexdocs.pm/elixir/Stream.html) module.
* The Erlang stream module from the [Datum
  library](https://github.com/fogfish/datum/blob/master/src/stream/stream.erl).
* The [zlist](https://github.com/egobrain/zlist) Erlang
  library.
* The [infinite lists](http://erlang.org/documentation/doc-5.8/doc/programming_examples/funs.html)
  example from the Erlang documentation.
