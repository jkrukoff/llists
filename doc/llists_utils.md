

# Module llists_utils #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Additional iterator utilities that are not replicas of `lists`
module functionality.

<a name="description"></a>

## Description ##
These functions are kept separate to avoid
any future name clashes with additions to the stdlib.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cycle-1">cycle/1</a></td><td>
Create an infinite iterator that repeatedly returns the sequence of
elements in the given iterator.</td></tr><tr><td valign="top"><a href="#group-2">group/2</a></td><td>
Create an iterator that returns groups of elements from <code>Iterator1</code>
as a list of at least <code>Length</code> elements.</td></tr><tr><td valign="top"><a href="#groupwith-2">groupwith/2</a></td><td>
Create an iterator that returns groups of elements from <code>Iterator1</code>
based on the return value of <code>Pred(Elem)</code>.</td></tr><tr><td valign="top"><a href="#unique-1">unique/1</a></td><td>
As <code>unique/2</code>, but with <code>==</code> as a equality function.</td></tr><tr><td valign="top"><a href="#unique-2">unique/2</a></td><td>
Discards repeated values in a sorted iterator according to a
provided equality function <code>Fun(A, B)</code> which should return <code>true</code>
when <code>A</code> and <code>B</code> are equal and <code>false</code> otherwise.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cycle-1"></a>

### cycle/1 ###

<pre><code>
cycle(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li></ul>

Create an infinite iterator that repeatedly returns the sequence of
elements in the given iterator.

<a name="group-2"></a>

### group/2 ###

<pre><code>
group(Length, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Length = pos_integer()</code></li><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>([Elem])</code></li></ul>

Create an iterator that returns groups of elements from `Iterator1`
as a list of at least `Length` elements.

It is not an error if there are not enough elements to fill out the
final group, instead a smaller group is returned.

<a name="groupwith-2"></a>

### groupwith/2 ###

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

If `Pred(Elem)` returns false for every element in an infinite
iterator, the first evaluation of `Iterator2` will never return.

<a name="unique-1"></a>

### unique/1 ###

<pre><code>
unique(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li></ul>

As `unique/2`, but with `==` as a equality function.

__See also:__ [unique/2](#unique-2).

<a name="unique-2"></a>

### unique/2 ###

<pre><code>
unique(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="llists.md#type-compare">llists:compare</a>(A, B)</code></li><li><code>Iterator1 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="llists.md#type-iterator">llists:iterator</a>(Elem)</code></li><li><code>A = Elem</code></li><li><code>B = Elem</code></li></ul>

Discards repeated values in a sorted iterator according to a
provided equality function `Fun(A, B)` which should return `true`
when `A` and `B` are equal and `false` otherwise. All values that
compares equal to the previously returned value are skipped until a
non-equal value is found.

Infinite iterators of equal values will cause the first evaluation
of `Iterator2` to never return.

