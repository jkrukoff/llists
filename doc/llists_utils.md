# Module llists_utils

- [Description](#description)
- [Data Types](#types)
- [Function Index](#index)
- [Function Details](#functions)

Additional iterator utilities that are not replicas of `lists`
module functionality.

<a name="description"></a>

## Description

These functions are kept separate to avoid
any future name clashes with additions to the stdlib.

Unlike the functions in `llists`, these utility functions do not
follow the same strict transformation rules. Instead, inputs and
outputs generally follow evaluation needs with eagerly evaluated
values passed as lists and lazily evaluated ones passed as
iterators.
<a name="types"></a>

## Data Types

### <a name="type-permutation_options">permutation_options()</a>

<pre><code>
permutation_options() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>

<a name="index"></a>

## Function Index

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#choice-1">choice/1</a></td><td>
Create an infinite iterator that returns random elements from the
given list of <code>Choices</code>.</td></tr><tr><td valign="top"><a href="#combinations-2">combinations/2</a></td><td></td></tr><tr><td valign="top"><a href="#combinations-3">combinations/3</a></td><td>
Create an iterator that returns all combinations of elements from
<code>Choices</code> that are <code>N</code> elements long.</td></tr><tr><td valign="top"><a href="#cycle-1">cycle/1</a></td><td>
Create an infinite iterator that repeatedly returns the sequence of
elements in the given iterator.</td></tr><tr><td valign="top"><a href="#enumerate-1">enumerate/1</a></td><td>
Given an existing <code>Iterator1</code> creates a new <code>Iterator2</code> which
returns each element of the original iterator as a tuple of the
number of elements returned and the element itself.</td></tr><tr><td valign="top"><a href="#group-2">group/2</a></td><td>
Create an iterator that returns groups of elements from <code>Iterator1</code>
as a list of at least <code>Length</code> elements.</td></tr><tr><td valign="top"><a href="#groupwith-2">groupwith/2</a></td><td>
Create an iterator that returns groups of elements from <code>Iterator1</code>
based on the return value of <code>Pred(Elem)</code>.</td></tr><tr><td valign="top"><a href="#permutations-2">permutations/2</a></td><td></td></tr><tr><td valign="top"><a href="#permutations-3">permutations/3</a></td><td>
Create an iterator that returns all permutations of elements from
<code>Choices</code> that are <code>N</code> elements long.</td></tr><tr><td valign="top"><a href="#random-0">random/0</a></td><td>
Create an infinite iterator that returns random floats in the range
<code>[0.0, 1.0)</code>.</td></tr><tr><td valign="top"><a href="#random-1">random/1</a></td><td>
Create an infinite iterator that returns random integers in the range
<code>[1, N)</code>.</td></tr><tr><td valign="top"><a href="#unique-1">unique/1</a></td><td>
As <code>unique/2</code>, but with <code>==</code> as a equality function.</td></tr><tr><td valign="top"><a href="#unique-2">unique/2</a></td><td>
Discards repeated values in a sorted iterator according to a
provided equality function <code>Fun(A, B)</code> which should return <code>true</code>
when <code>A</code> and <code>B</code> are equal and <code>false</code> otherwise.</td></tr></table>

<a name="functions"></a>

## Function Details

<a name="choice-1"></a>

### choice/1

<pre><code>
choice(Choices) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>Choices = [Elem, ...]</code></li><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li></ul>

Create an infinite iterator that returns random elements from the
given list of `Choices`. Each iterator returns a unique sequence
and returns the same unique sequence each time it is evaluated.

<a name="combinations-2"></a>

### combinations/2

<pre><code>
combinations(N, Choices) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>Choices = [Elem]</code></li><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

**See also:** [combinations/3](#combinations-3).

<a name="combinations-3"></a>

### combinations/3

<pre><code>
combinations(N, Choices, Options) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>Choices = [Elem]</code></li><li><code>Options = <a href="#type-permutation_options">permutation_options()</a></code></li><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

Create an iterator that returns all combinations of elements from
`Choices` that are `N` elements long. If the `repetitions` property
is passed in `Options`, combinations with repeated elements of
`Choices` are included.

Examples:

```
  > llists:to_list(
       llists_utils:combinations(2, [1, 2, 3]).
  [[1,2],[1,3],[2,3]]
  > llists:to_list(
       llists_utils:combinations(2, [1, 2, 3], [repetitions]).
  [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
```

If the elements of `Choices` are sorted, the order of the resulting
combinations will also be sorted.

<a name="cycle-1"></a>

### cycle/1

<pre><code>
cycle(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li></ul>

Create an infinite iterator that repeatedly returns the sequence of
elements in the given iterator.

<a name="enumerate-1"></a>

### enumerate/1

<pre><code>
enumerate(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>({Index, Elem})</code></li><li><code>Index = pos_integer()</code></li></ul>

Given an existing `Iterator1` creates a new `Iterator2` which
returns each element of the original iterator as a tuple of the
number of elements returned and the element itself.

Example:

```
  > llists:to_list(
       llists_utils:enumerate(
           llits:from_list([one, two, three]))).
  [{1,one},{2,two},{3,three}]
```

<a name="group-2"></a>

### group/2

<pre><code>
group(Length, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Length = pos_integer()</code></li><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

Create an iterator that returns groups of elements from `Iterator1`
as a list of at least `Length` elements.

Example:

```
  > llists:to_list(
       llists_utils:group(
           2,
           llists:from_list([1, 2, 3, 4, 5]))).
  [[1,2],[3,4],[5]]
```

It is not an error if there are not enough elements to fill out the
final group, instead a smaller group is returned.

<a name="groupwith-2"></a>

### groupwith/2

<pre><code>
groupwith(Pred, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="llists.md#type-predicate">llists:predicate</a>(Elem)</code></li><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

Create an iterator that returns groups of elements from `Iterator1`
based on the return value of `Pred(Elem)`. If the predicate
function returns `true` it signals the end of a group which will be
returned as a list. If the predicate returns `false`, the element
will be included in the next group returned. Even if the predicate
function returns `false` for the last element, the final group will
still be returned.

Example:

```
  > llists:to_list(
       llists_utils:groupwith(
           fun (Elem) -> Elem rem 2 == 0 end,
           llists:from_list([1, 2, 3, 4, 5]))).
  [[1,2],[3,4],[5]]
```

If `Pred(Elem)` returns false for every element in an infinite
iterator, the first evaluation of `Iterator2` will never return.

<a name="permutations-2"></a>

### permutations/2

<pre><code>
permutations(N, Choices) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>Choices = [Elem]</code></li><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

**See also:** [permutations/3](#permutations-3).

<a name="permutations-3"></a>

### permutations/3

<pre><code>
permutations(N, Choices, Options) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>Choices = [Elem]</code></li><li><code>Options = <a href="#type-permutation_options">permutation_options()</a></code></li><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

Create an iterator that returns all permutations of elements from
`Choices` that are `N` elements long. If the `repetitions` property
is passed in `Options`, permutations with repeated elements of
`Choices` are included.

Examples:

```
  > llists:to_list(
       llists_utils:permutations(2, [1, 2, 3]).
  [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
  > llists:to_list(
       llists_utils:permutations(2, [1, 2, 3], [repetitions]).
  [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
```

If the elements of `Choices` are sorted, the order of the resulting
permutations will also be sorted.

<a name="random-0"></a>

### random/0

<pre><code>
random() -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>(float())</code></li></ul>

Create an infinite iterator that returns random floats in the range
`[0.0, 1.0)`. Each iterator returns a unique sequence and returns
the same unique sequence each time it is evaluated.

**See also:** [rand:uniform/0](rand.md#uniform-0).

<a name="random-1"></a>

### random/1

<pre><code>
random(N) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>Iterator = <a href="llists.md#type-iterator">llists:iterator</a>(float())</code></li></ul>

Create an infinite iterator that returns random integers in the range
`[1, N)`. Each iterator returns a unique sequence and returns
the same unique sequence each time it is evaluated.

**See also:** [rand:uniform/1](rand.md#uniform-1).

<a name="unique-1"></a>

### unique/1

<pre><code>
unique(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li></ul>

As `unique/2`, but with `==` as a equality function.

**See also:** [unique/2](#unique-2).

<a name="unique-2"></a>

### unique/2

<pre><code>
unique(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="llists.md#type-compare">llists:compare</a>(A, B)</code></li><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>A = Elem</code></li><li><code>B = Elem</code></li></ul>

Discards repeated values in a sorted iterator according to a
provided equality function `Fun(A, B)` which should return `true`
when `A` and `B` are equal and `false` otherwise. All values that
compares equal to the previously returned value are skipped until a
non-equal value is found.

Example:

```
  > llists:to_list(
       llists_utils:unique(
           llists:from_list([1, 1, 2, 2, 1, 1]))).
  [1,2,1]
```

Infinite iterators of equal values will cause the first evaluation
of `Iterator2` to never return.
