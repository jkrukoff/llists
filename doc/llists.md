# Module llists

- [Description](#description)
- [Data Types](#types)
- [Function Index](#index)
- [Function Details](#functions)

A lazily evaluated lists module.

<a name="description"></a>

## Description

This module provides an iterator
type which is an opaque record wrapped around a list continuation.
These iterators are then used to provide a version of the stdlib
`lists` functions which only evaluate elements of the iterator
when demanded.

Several simple iterator constructors are provided as well as a
general purpose `unfold/2` constructor. Conversion to built in
types for lists and maps as the `from_list/1`, `to_list/1`,
`from_map/1` and `to_map/1` functions.

Iterators are evaluated using the `next/1` function to evaluate
the next element of the iterator. The output of the next function
is a lazy list: either an improper list of the element and the
continuation or an empty list. Many additional iterator
transformation and evaluation functions are also present.

In general, iterators are not expected to be pure functions.
Iterator transformations and evaluations should all evaluate each
element exactly once per output iterator (though not all elements
may be returned, depending on the function). This implies that
impure iterators should not be used with functions which return
multiple iterators if all iterators are to be evaluated.

Many of the functions here are unsafe to use with infinite
iterators and will either fail to return on the initial call or on
the first attempt to evaluate an element of the iterator. Read the
documentation carefully when working with such iterators.

The interface for this module attempts to follow the `lists`
behaviour as closely as possible. Guidelines for how past and
future translation is performed is as follows:

- Any input lists are changed to expect iterators.

- Any output lists are changed to be iterators.

- Elements of input iterators should be evaluated exactly
  once per output iterator.

- Any numeric counts for repetition are changed to allow
  'infinity' as values and to be able to return infinite
  iterators.

- On error, the same exception should be raised, though it may
  not be raised until the triggering element of an iterator is
  evaluated.

- Iteration evaluation behaviour is documented.

As few functions outside of `lists` have been implemented as
possible in order to have the best chance of keeping the namespace
clean for future additions to the `lists` module. New
functionality is instead implemented in the `llists_utils` module.
<a name="types"></a>

## Data Types

### <a name="type-accumulator">accumulator()</a>

<pre><code>
accumulator() = any()
</code></pre>

### <a name="type-combine">combine()</a>

<pre><code>
combine(A, B, Out) = fun((A, B) -&gt; Out)
</code></pre>

### <a name="type-combine3">combine3()</a>

<pre><code>
combine3(A, B, C, Out) = fun((A, B, C) -&gt; Out)
</code></pre>

### <a name="type-compare">compare()</a>

<pre><code>
compare(A, B) = fun((A, B) -&gt; boolean())
</code></pre>

### <a name="type-filtermap">filtermap()</a>

<pre><code>
filtermap(A, B) = fun((A) -&gt; boolean() | {true, B})
</code></pre>

### <a name="type-fold">fold()</a>

<pre><code>
fold(Elem, AccIn, AccOut) = fun((Elem, AccIn) -&gt; AccOut)
</code></pre>

### <a name="type-iterator">iterator()</a>

**abstract datatype**: `iterator(Over)`

### <a name="type-iterator">iterator()</a>

<pre><code>
iterator() = <a href="#type-iterator">iterator</a>(any())
</code></pre>

### <a name="type-lazy_list">lazy_list()</a>

<pre><code>
lazy_list(Over) = nonempty_improper_list(Over, <a href="#type-iterator">iterator</a>(Over)) | []
</code></pre>

### <a name="type-map">map()</a>

<pre><code>
map(A, B) = fun((A) -&gt; B)
</code></pre>

### <a name="type-mapfold">mapfold()</a>

<pre><code>
mapfold(A, AccIn, B, AccOut) = fun((A, AccIn) -&gt; {B, AccOut})
</code></pre>

### <a name="type-predicate">predicate()</a>

<pre><code>
predicate(Elem) = fun((Elem) -&gt; boolean())
</code></pre>

### <a name="type-tuple_iterator">tuple_iterator()</a>

<pre><code>
tuple_iterator() = <a href="#type-iterator">iterator</a>(tuple())
</code></pre>

### <a name="type-unfold">unfold()</a>

<pre><code>
unfold(Elem, AccIn, AccOut) = fun((AccIn) -&gt; {Elem, AccOut} | none)
</code></pre>

<a name="index"></a>

## Function Index

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-2">all/2</a></td><td>
Returns <code>true</code> if <code>Pred(Elem)</code> returns <code>true</code> for all elements
<code>Elem</code> in <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#any-2">any/2</a></td><td>
Returns <code>true</code> if <code>Pred(Elem)</code> returns <code>true</code> for at least one
element <code>Elem</code> in <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#append-1">append/1</a></td><td>
Returns an iterator in which all the subiterators of
<code>IteratorOfIterators</code> have been appended.</td></tr><tr><td valign="top"><a href="#append-2">append/2</a></td><td>
Returns a new iterator <code>Iterator3</code>, which is made from the elements
of <code>Iterator1</code> followed by the elements of <code>Iterator2</code>.</td></tr><tr><td valign="top"><a href="#concat-1">concat/1</a></td><td>
Concatenates the text representation of the elements of <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>
Returns a copy of <code>Iterator1</code> where the first element matching
<code>Elem</code> is deleted, if there is such an element.</td></tr><tr><td valign="top"><a href="#droplast-1">droplast/1</a></td><td>
Drops the last element of a <code>Iterator1</code>.</td></tr><tr><td valign="top"><a href="#dropwhile-2">dropwhile/2</a></td><td>
Drops elements <code>Elem</code> from <code>Iterator1</code> while <code>Pred(Elem)</code> returns
true and returns the remaining iterator.</td></tr><tr><td valign="top"><a href="#duplicate-2">duplicate/2</a></td><td>
Returns an iterator containing <code>N</code> copies of term <code>Elem</code>.</td></tr><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td>
<code>Filtered</code> is an iterator of all elements <code>Elem</code> in <code>Iterator1</code> for
which <code>Pred(Elem)</code> returns <code>true</code>.</td></tr><tr><td valign="top"><a href="#filtermap-2">filtermap/2</a></td><td>
Calls <code>Fun(Elem)</code> on successive elements <code>Elem</code> of <code>Iterator1</code>.</td></tr><tr><td valign="top"><a href="#flatlength-1">flatlength/1</a></td><td>
Equivalent to <code>length(flatten(DeepIterator))</code>.</td></tr><tr><td valign="top"><a href="#flatmap-2">flatmap/2</a></td><td>
Takes a function from <code>A</code>s to iterators of <code>B</code>s, and an iterator of
<code>A</code>s (<code>Iterator1</code>) and produces an iterator of <code>B</code>s (<code>Iterator2</code>)
by applying the function to every element in <code>Iterator1</code> and
appending the resulting iterators.</td></tr><tr><td valign="top"><a href="#flatten-1">flatten/1</a></td><td>
Returns a flattened version of <code>DeepIterator</code>.</td></tr><tr><td valign="top"><a href="#flatten-2">flatten/2</a></td><td>
Returns a flattened version of <code>DeepIterator</code> with tail <code>Tail</code>
appended.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td>
Calls <code>Fun(Elem, AccIn)</code> on successive elements <code>A</code> of <code>Iterator</code>,
starting with <code>AccIn</code> == <code>Acc0</code>.</td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td>
Like <code>foldl/3</code>, but the list is traversed from right to left.</td></tr><tr><td valign="top"><a href="#foreach-2">foreach/2</a></td><td>
Calls <code>Fun(Elem)</code> for each element <code>Elem</code> in <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>
Construct a new iterator from an existing list.</td></tr><tr><td valign="top"><a href="#from_map-1">from_map/1</a></td><td>
Construct a new iterator from an existing map.</td></tr><tr><td valign="top"><a href="#hd-1">hd/1</a></td><td>
Returns the head of <code>Iterator</code>, that is, the first element, for
example:.</td></tr><tr><td valign="top"><a href="#is_iterator-1">is_iterator/1</a></td><td>
Tests if the given <code>Candidate</code> is an iterator, returns <code>true</code> if it
and <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>
Inserts <code>Sep</code> between each element in <code>Iterator1</code>.</td></tr><tr><td valign="top"><a href="#keydelete-3">keydelete/3</a></td><td>
Returns a copy of <code>TupleIterator1</code> where the first occurrence of a tuple
whose <code>N</code>th element compares equal to <code>Key</code> is deleted, if there is
such a tuple.</td></tr><tr><td valign="top"><a href="#keyfind-3">keyfind/3</a></td><td>
Searches the iterator of tuples <code>TupleIterator</code> for a tuple whose
<code>N</code>th element compares equal to <code>Key</code>.</td></tr><tr><td valign="top"><a href="#keymap-3">keymap/3</a></td><td>
Returns an iterator of tuples where, for each tuple in
<code>TupleIterator1</code>, the <code>N</code>th element <code>Term1</code> of the tuple has been
replaced with the result of calling <code>Fun(Term1)</code>.</td></tr><tr><td valign="top"><a href="#keymember-3">keymember/3</a></td><td>
Returns <code>true</code> if there is a tuple in <code>TupleIterator</code> whose <code>N</code>th
element compares equal to <code>Key</code>, otherwise <code>false</code>.</td></tr><tr><td valign="top"><a href="#keymerge-3">keymerge/3</a></td><td>
Returns the sorted iterator formed by merging <code>TupleIterator1</code> and
<code>TupleIterator2</code>.</td></tr><tr><td valign="top"><a href="#keyreplace-4">keyreplace/4</a></td><td>
Returns a copy of <code>TupleIterator1</code> where the first occurrence of a T
tuple whose <code>N</code>th element compares equal to <code>Key</code> is replaced with
<code>NewTuple</code>, if there is such a tuple <code>T</code>.</td></tr><tr><td valign="top"><a href="#keysearch-3">keysearch/3</a></td><td>
Searches the iterator of tuples <code>TupleIterator</code> for a tuple whose
<code>N</code>th element compares equal to <code>Key</code>.</td></tr><tr><td valign="top"><a href="#keysort-2">keysort/2</a></td><td>
Returns an iterator containing the sorted elements of iterator
<code>TupleIterator1</code>.</td></tr><tr><td valign="top"><a href="#keystore-4">keystore/4</a></td><td>
Returns a copy of <code>TupleIterator1</code> where the first occurrence of a
tuple <code>T</code> whose <code>N</code>th element compares equal to <code>Key</code> is replaced
with <code>NewTuple</code>, if there is such a tuple <code>T</code>.</td></tr><tr><td valign="top"><a href="#keytake-3">keytake/3</a></td><td>
Searches the iterator of tuples <code>TupleIterator1</code> for a tuple whose
<code>N</code>th element compares equal to <code>Key</code>.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td>
Returns the last element in <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td>
Returns the length of <code>Iterator</code>, for example:.</td></tr><tr><td valign="top"><a href="#map-2">map/2</a></td><td>
Takes a function <code>Fun</code> from <code>A</code>s to <code>B</code>s, and an <code>Iterator1</code> of
<code>A</code>s and produces an <code>Iterator2</code> of <code>B</code>s by applying the function
to every element in the iterator.</td></tr><tr><td valign="top"><a href="#mapfoldl-3">mapfoldl/3</a></td><td>
Combines the operations of <code>map/2</code> and <code>foldl/3</code> into one pass.</td></tr><tr><td valign="top"><a href="#mapfoldr-3">mapfoldr/3</a></td><td>
Combines the operations of map/2 and foldr/3 into one pass.</td></tr><tr><td valign="top"><a href="#max-1">max/1</a></td><td>
Returns the first element of <code>Iterator</code> that compares greater than
or equal to all other elements of <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td>
Returns <code>true</code> if <code>Elem</code> matches some element of <code>Iterator</code>,
otherwise <code>false</code>.</td></tr><tr><td valign="top"><a href="#merge-1">merge/1</a></td><td>
Returns the sorted iterator formed by merging all the subiterators
of <code>IteratorOfIterators</code>.</td></tr><tr><td valign="top"><a href="#merge-2">merge/2</a></td><td>
Returns the sorted iterator formed by merging <code>Iterator1</code> and
<code>Iterator2</code>.</td></tr><tr><td valign="top"><a href="#merge-3">merge/3</a></td><td>
Returns the sorted iterator formed by merging <code>Iterator1</code> and
<code>Iterator2</code>.</td></tr><tr><td valign="top"><a href="#merge3-3">merge3/3</a></td><td>
Returns the sorted iterator formed by merging <code>Iterator1</code>,
<code>Iterator2</code>, and <code>Iterator3</code>.</td></tr><tr><td valign="top"><a href="#min-1">min/1</a></td><td>
Returns the first element of <code>Iterator</code> that compares less than or
equal to all other elements of <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#next-1">next/1</a></td><td>
Demand an element from <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#nth-2">nth/2</a></td><td>
Returns the <code>N</code>th element of <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#nthtail-2">nthtail/2</a></td><td>
Returns the <code>N</code>th tail of <code>Iterator1</code>, that is, the subiterator of
<code>Iterator1</code> starting at <code>N</code>+1 and continuing up to the end of the
iterator.</td></tr><tr><td valign="top"><a href="#partition-2">partition/2</a></td><td>
Partitions <code>Iterator1</code> into two iterators, where the first iterator
contains all elements for which <code>Pred(Elem)</code> returns <code>true</code>, and
the second iterator contains all elements for which <code>Pred(Elem)</code>
returns <code>false</code>.</td></tr><tr><td valign="top"><a href="#prefix-2">prefix/2</a></td><td>
Returns <code>true</code> if <code>Iterator1</code> is a prefix of <code>Iterator2</code>, otherwise <code>false</code>.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>
Returns an iterator with the elements in <code>Iterator1</code> in reverse
order.</td></tr><tr><td valign="top"><a href="#reverse-2">reverse/2</a></td><td>
Returns a list with the elements in <code>Iterator1</code> in reverse order,
with tail <code>TailIterator</code> appended.</td></tr><tr><td valign="top"><a href="#search-2">search/2</a></td><td>
If there is a <code>Value</code> in <code>Iterator</code> such that <code>Pred(Value)</code> returns <code>true</code>,
returns <code>{value, Value}</code> for the first such <code>Value</code>, otherwise returns
<code>false</code>.</td></tr><tr><td valign="top"><a href="#seq-2">seq/2</a></td><td></td></tr><tr><td valign="top"><a href="#seq-3">seq/3</a></td><td>
Returns an iterator over a sequence of integers that starts with
<code>From</code> and contains the successive results of adding <code>Incr</code> to the
previous element, until <code>To</code> is reached or passed (in the latter
case, <code>To</code> is not an element of the sequence).</td></tr><tr><td valign="top"><a href="#sort-1">sort/1</a></td><td>
Returns an iterator containing the sorted elements of <code>Iterator1</code>.</td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td>
Returns an iterator containing the sorted elements of <code>Iterator1</code>,
according to the ordering function <code>Fun</code>.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td>
Splits <code>Iterator1</code> into <code>Iterator2</code> and <code>Iterator3</code>.</td></tr><tr><td valign="top"><a href="#splitwith-2">splitwith/2</a></td><td>
Partitions <code>Iterator1</code> into two iterators according to <code>Pred</code>.</td></tr><tr><td valign="top"><a href="#sublist-2">sublist/2</a></td><td></td></tr><tr><td valign="top"><a href="#sublist-3">sublist/3</a></td><td>
Returns the portion of <code>Iterator1</code> starting at <code>Start</code> and with
(maximum) <code>Len</code> elements.</td></tr><tr><td valign="top"><a href="#subtract-2">subtract/2</a></td><td>
Returns a new iterator <code>Iterator3</code> that is a copy of <code>Iterator1</code>,
subjected to the following procedure: for each element in
<code>Iterator2</code>, its first occurrence in <code>Iterator1</code> is deleted.</td></tr><tr><td valign="top"><a href="#suffix-2">suffix/2</a></td><td>
Returns <code>true</code> if <code>Iterator1</code> is a suffix of <code>Iterator2</code>, otherwise
<code>false</code>.</td></tr><tr><td valign="top"><a href="#sum-1">sum/1</a></td><td>
Returns the sum of the elements in <code>Iterator</code>.</td></tr><tr><td valign="top"><a href="#takewhile-2">takewhile/2</a></td><td>
Takes elements <code>Elem</code> from <code>Iterator1</code> while <code>Pred(Elem)</code> returns
true, that is, the function returns the longest prefix of the
iterator for which all elements satisfy the predicate.</td></tr><tr><td valign="top"><a href="#tl-1">tl/1</a></td><td>
Returns the tail of <code>Iterator1</code>, that is, the iterator minus the
first element, for example:.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>
Fully evaluate <code>Iterator</code> and return a list containing all elements
produced.</td></tr><tr><td valign="top"><a href="#to_map-1">to_map/1</a></td><td>
Fully evaluate an <code>Iterator</code> of <code>{Key, Value}</code> tuples and return a
map containing all pairs produced.</td></tr><tr><td valign="top"><a href="#ukeymerge-3">ukeymerge/3</a></td><td>
Returns the sorted iterator formed by merging <code>TupleIterator1</code> and
<code>TupleIterator2</code>.</td></tr><tr><td valign="top"><a href="#ukeysort-2">ukeysort/2</a></td><td>
Returns a iterator containing the sorted elements of iterator
<code>TupleIterator1</code> where all except the first tuple of the tuples
comparing equal have been deleted.</td></tr><tr><td valign="top"><a href="#umerge-1">umerge/1</a></td><td>
Returns the sorted iterator formed by merging all the subiterators of
<code>IteratorOfIterators</code>.</td></tr><tr><td valign="top"><a href="#umerge-2">umerge/2</a></td><td>
Returns the sorted iterator formed by merging <code>Iterator1</code> and
<code>Iterator2</code>.</td></tr><tr><td valign="top"><a href="#umerge-3">umerge/3</a></td><td>
Returns the sorted iterator formed by merging <code>Iterator1</code> and
<code>Iterator2</code>.</td></tr><tr><td valign="top"><a href="#umerge3-3">umerge3/3</a></td><td>
Returns the sorted iterator formed by merging <code>Iterator1</code>,
<code>Iterator2</code>, and <code>Iterator3</code>.</td></tr><tr><td valign="top"><a href="#unfold-2">unfold/2</a></td><td>
Construct a new iterator from a <code>Fun(AccIn)</code> function and an
initial accumulator value <code>Acc0</code>.</td></tr><tr><td valign="top"><a href="#unzip-1">unzip/1</a></td><td>
"Unzips" a iterator of two-tuples into two iterators, where the
first iterator contains the first element of each tuple, and the
second iterator contains the second element of each tuple.</td></tr><tr><td valign="top"><a href="#unzip3-1">unzip3/1</a></td><td>
"Unzips" a iterator of three-tuples into three iterators, where the first
iterator contains the first element of each tuple, the second iterator
contains the second element of each tuple, and the third iterator
contains the third element of each tuple.</td></tr><tr><td valign="top"><a href="#usort-1">usort/1</a></td><td>
Returns a iterator containing the sorted elements of <code>Iterator1</code>
where all except the first element of the elements comparing equal
have been deleted.</td></tr><tr><td valign="top"><a href="#usort-2">usort/2</a></td><td>
Returns a iterator containing the sorted elements of <code>Iterator1</code> where all
except the first element of the elements comparing equal according
to the ordering function <code>Fun</code> have been deleted.</td></tr><tr><td valign="top"><a href="#zip-2">zip/2</a></td><td>
"Zips" two iterators of equal length into one iterator of
two-tuples, where the first element of each tuple is taken from the
first iterator and the second element is taken from the
corresponding element in the second iterator.</td></tr><tr><td valign="top"><a href="#zip3-3">zip3/3</a></td><td>
"Zips" three iterators of equal length into one iterator of
three-tuples, where the first element of each tuple is taken from
the first iterator, the second element is taken from the
corresponding element in the second iterator, and the third element
is taken from the corresponding element in the third iterator.</td></tr><tr><td valign="top"><a href="#zipwith-3">zipwith/3</a></td><td>
Combines the elements of two iterators of equal length into one iterator.</td></tr><tr><td valign="top"><a href="#zipwith3-4">zipwith3/4</a></td><td>
Combines the elements of three iterators of equal length into one
iterator.</td></tr></table>

<a name="functions"></a>

## Function Details

<a name="all-2"></a>

### all/2

<pre><code>
all(Pred, Iterator) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns `true` if `Pred(Elem)` returns `true` for all elements
`Elem` in `Iterator`.

Stops evaluating `Iterator` when `Pred(Elem)` returns `false` or
when `Iterator` is empty.

<a name="any-2"></a>

### any/2

<pre><code>
any(Pred, Iterator) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns `true` if `Pred(Elem)` returns `true` for at least one
element `Elem` in `Iterator`.

Stops evaluating `Iterator` when `Pred(Elem)` returns `true` or
when `Iterator` is empty.

<a name="append-1"></a>

### append/1

<pre><code>
append(IteratorOfIterators) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IteratorOfIterators = <a href="#type-iterator">iterator</a>(<a href="#type-iterator">iterator()</a>)</code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns an iterator in which all the subiterators of
`IteratorOfIterators` have been appended.

<a name="append-2"></a>

### append/2

<pre><code>
append(Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem1)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem2)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(Elem1 | Elem2)</code></li></ul>

Returns a new iterator `Iterator3`, which is made from the elements
of `Iterator1` followed by the elements of `Iterator2`.

<a name="concat-1"></a>

### concat/1

<pre><code>
concat(Iterator) -&gt; string()
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(atom() | integer() | float() | string())</code></li></ul>

Concatenates the text representation of the elements of `Iterator`.
The elements of `Iterator` can be atoms, integers, floats, or
strings. The iterator will be fully evaluated, infinite iterators
will never return.

<a name="delete-2"></a>

### delete/2

<pre><code>
delete(Elem1, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Elem1 = any()</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem2)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem2)</code></li><li><code>Elem2 = any()</code></li></ul>

Returns a copy of `Iterator1` where the first element matching
`Elem` is deleted, if there is such an element.

<a name="droplast-1"></a>

### droplast/1

<pre><code>
droplast(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Drops the last element of a `Iterator1`. The `Iterator1` is to be
non-empty, otherwise the function crashes with a `function_clause`.

Evaluates one element further in the iterator than the current
value.

<a name="dropwhile-2"></a>

### dropwhile/2

<pre><code>
dropwhile(Pred, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Drops elements `Elem` from `Iterator1` while `Pred(Elem)` returns
true and returns the remaining iterator.

<a name="duplicate-2"></a>

### duplicate/2

<pre><code>
duplicate(N, Elem) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>N = infinity | non_neg_integer()</code></li><li><code>Elem = any()</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns an iterator containing `N` copies of term `Elem`. If `N` is
`infinity` iterator will return infinite copies of `Elem`.

<a name="filter-2"></a>

### filter/2

<pre><code>
filter(Pred, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

`Filtered` is an iterator of all elements `Elem` in `Iterator1` for
which `Pred(Elem)` returns `true`.

<a name="filtermap-2"></a>

### filtermap/2

<pre><code>
filtermap(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-filtermap">filtermap</a>(Elem, Value)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem | Value)</code></li></ul>

Calls `Fun(Elem)` on successive elements `Elem` of `Iterator1`.
`Fun/1` must return either a Boolean or a tuple `{true, Value}`.
The function returns the iterator of elements for which `Fun`
returns a new value, where a value of `true` is synonymous with
`{true, Elem}`.

That is, filtermap behaves as if it had been defined as follows,
except that the iterator is not fully evaluated before elements are
returned:

```
  filtermap(Fun, Iterator) ->
      llists:foldr(fun(Elem, Acc) ->
                          case Fun(Elem) of
                              false -> Acc;
                              true -> [Elem|Acc];
                              {true,Value} -> [Value|Acc]
                          end
                   end, [], Iterator).
```

Example:

```
  > llists:to_list(
  >  llists:filtermap(
  >   fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end,
  >   llists:seq(1, 5))).
  [1,2]
```

<a name="flatlength-1"></a>

### flatlength/1

<pre><code>
flatlength(DeepIterator) -&gt; Length
</code></pre>

<ul class="definitions"><li><code>DeepIterator = <a href="#type-iterator">iterator</a>(any() | <a href="#type-iterator">iterator()</a>)</code></li><li><code>Length = non_neg_integer()</code></li></ul>

Equivalent to `length(flatten(DeepIterator))`.

<a name="flatmap-2"></a>

### flatmap/2

<pre><code>
flatmap(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-map">map</a>(A, <a href="#type-iterator">iterator</a>(B))</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li></ul>

Takes a function from `A`s to iterators of `B`s, and an iterator of
`A`s (`Iterator1`) and produces an iterator of `B`s (`Iterator2`)
by applying the function to every element in `Iterator1` and
appending the resulting iterators.

That is, flatmap behaves as if it had been defined as follows:

```
  llists:flatmap(Fun, Iterator) ->
      llists:append(llists:map(Fun, Iterator)).
```

Example:

```
  > llists:to_list(
  >  llists:flatmap(
  >   fun(X)->llists:from_list([X,X]) end,
  >   llists:from_list([a,b,c]))).
  [a,a,b,b,c,c]
```

<a name="flatten-1"></a>

### flatten/1

<pre><code>
flatten(DeepIterator) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>DeepIterator = <a href="#type-iterator">iterator</a>(any() | <a href="#type-iterator">iterator()</a>)</code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns a flattened version of `DeepIterator`.

<a name="flatten-2"></a>

### flatten/2

<pre><code>
flatten(DeepIterator, TailIterator) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>DeepIterator = <a href="#type-iterator">iterator</a>(any() | <a href="#type-iterator">iterator()</a>)</code></li><li><code>TailIterator = <a href="#type-iterator">iterator()</a></code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns a flattened version of `DeepIterator` with tail `Tail`
appended.

<a name="foldl-3"></a>

### foldl/3

<pre><code>
foldl(Fun, Acc0, Iterator) -&gt; AccOut
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-fold">fold</a>(A, AccIn::Acc0 | AccOut, AccOut)</code></li><li><code>Acc0 = any()</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(A)</code></li></ul>

Calls `Fun(Elem, AccIn)` on successive elements `A` of `Iterator`,
starting with `AccIn` == `Acc0`. `Fun/2` must return a new
accumulator, which is passed to the next call. The function returns
the final value of the accumulator. `Acc0` is returned if the
iterator is empty.

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="foldr-3"></a>

### foldr/3

<pre><code>
foldr(Fun, Acc0, Iterator) -&gt; AccOut
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-fold">fold</a>(A, AccIn::Acc0 | AccOut, AccOut)</code></li><li><code>Acc0 = any()</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(A)</code></li></ul>

Like `foldl/3`, but the list is traversed from right to left.

Example:

```
  > P = fun(A, AccIn) -> io:format("~p ", [A]), AccIn end.
  #Fun<erl_eval.12.2225172>
  > llists:foldl(P, void, llists:seq(1, 3)).
  1 2 3 void
  > lists:foldr(P, void, llists:seq(1, 3)).
  3 2 1 void
```

The iterator is fully evaluated before the fold begins, infinite
iterators will never return. `foldl/3` does not fully evaluate the
iterator and is usually preferred to `foldr/3`.

**See also:** [foldl/3](#foldl-3).

<a name="foreach-2"></a>

### foreach/2

<pre><code>
foreach(Fun, Iterator) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-map">map</a>(Elem, any())</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Calls `Fun(Elem)` for each element `Elem` in `Iterator`. This
function is used for its side effects and the evaluation order is
defined to be the same as the order of the elements in the
iterator.

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="from_list-1"></a>

### from_list/1

<pre><code>
from_list(List) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>List = [Elem]</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Construct a new iterator from an existing list. Each element of the
list will be returned in order by the returned iterator.

<a name="from_map-1"></a>

### from_map/1

<pre><code>
from_map(Map) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>Map = <a href="maps.md#type-map">maps:map</a>(Key, Value)</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>({Key, Value})</code></li></ul>

Construct a new iterator from an existing map. Each `{Key, Value}`
tuple of the map will be returned in an arbitrary order by the
returned iterator.

<a name="hd-1"></a>

### hd/1

<pre><code>
hd(Iterator) -&gt; Elem
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the head of `Iterator`, that is, the first element, for
example:

```
  > llists:hd(llists:seq(1, 5)).
  1
```

Failure: `badarg` if `Iterator` is empty.

<a name="is_iterator-1"></a>

### is_iterator/1

<pre><code>
is_iterator(Candidate) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Candidate = any()</code></li></ul>

Tests if the given `Candidate` is an iterator, returns `true` if it
and `false` otherwise.

<a name="join-2"></a>

### join/2

<pre><code>
join(Sep, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Sep = any()</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Sep | Elem)</code></li></ul>

Inserts `Sep` between each element in `Iterator1`. Has no effect on
an empty iterator or on a singleton iterator. For example:

```
  > llists:to_list(llists:join(x, llists:from_list([a,b,c]))).
  [a,x,b,x,c]
  > llists:to_list(lists:join(x, llists:from_list([a]))).
  [a]
  > llists:to_list(lists:join(x, llists:from_list([]))).
  []
```

Evaluates one element further in the iterator than the current
value.

<a name="keydelete-3"></a>

### keydelete/3

<pre><code>
keydelete(Key, N, TupleIterator1) -&gt; TupleIterator2
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns a copy of `TupleIterator1` where the first occurrence of a tuple
whose `N`th element compares equal to `Key` is deleted, if there is
such a tuple.

<a name="keyfind-3"></a>

### keyfind/3

<pre><code>
keyfind(Key, N, TupleIterator) -&gt; Tuple | false
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator = <a href="#type-iterator">iterator()</a></code></li><li><code>Tuple = tuple() | false</code></li></ul>

Searches the iterator of tuples `TupleIterator` for a tuple whose
`N`th element compares equal to `Key`. Returns `Tuple` if such a
tuple is found, otherwise `false`.

The iterator will be evaluated until a match is found. If no match
is found, infinite iterators will never return.

<a name="keymap-3"></a>

### keymap/3

<pre><code>
keymap(Fun, N, TupleIterator1) -&gt; TupleIterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-map">map</a>(Term1::any(), Term2::any())</code></li><li><code>N = pos_integer</code></li><li><code>TupleIterator1 = <a href="#type-tuple_iterator">tuple_iterator()</a></code></li><li><code>TupleIterator2 = <a href="#type-tuple_iterator">tuple_iterator()</a></code></li></ul>

Returns an iterator of tuples where, for each tuple in
`TupleIterator1`, the `N`th element `Term1` of the tuple has been
replaced with the result of calling `Fun(Term1)`.

Examples:

```
  > Fun = fun(Atom) -> atom_to_list(Atom) end.
  #Fun<erl_eval.6.10732646>
  2> llists:to_list(
  2>  llists:keymap(
  2>   Fun,
  2>   2,
  2>   llists:from_list([{name,jane,22},{name,lizzie,20},{name,lydia,15}]))).
  [{name,"jane",22},{name,"lizzie",20},{name,"lydia",15}]
```

<a name="keymember-3"></a>

### keymember/3

<pre><code>
keymember(Key, N, TupleIterator) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns `true` if there is a tuple in `TupleIterator` whose `N`th
element compares equal to `Key`, otherwise `false`.

The iterator will be evaluated until a match is found. If no match
is found, infinite iterators will never return.

<a name="keymerge-3"></a>

### keymerge/3

<pre><code>
keymerge(N, TupleIterator1, TupleIterator2) -&gt; TupleIterator3
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem1)</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem2)</code></li><li><code>TupleIterator3 = <a href="#type-iterator">iterator</a>(Elem1 | Elem2)</code></li><li><code>Elem1 = tuple()</code></li><li><code>Elem2 = tuple()</code></li></ul>

Returns the sorted iterator formed by merging `TupleIterator1` and
`TupleIterator2`. The merge is performed on the `N`th element of
each tuple. Both `TupleIterator1` and `TupleIterator2` must be
key-sorted before evaluating this function. When two tuples compare
equal, the tuple from `TupleIterator1` is picked before the tuple
from `TupleIterator2`.

The first element of each iterator will be evaluated.

<a name="keyreplace-4"></a>

### keyreplace/4

<pre><code>
keyreplace(Key, N, TupleIterator1, NewTuple) -&gt; TupleIterator2
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>NewTuple = tuple()</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem | NewTuple)</code></li></ul>

Returns a copy of `TupleIterator1` where the first occurrence of a T
tuple whose `N`th element compares equal to `Key` is replaced with
`NewTuple`, if there is such a tuple `T`.

<a name="keysearch-3"></a>

### keysearch/3

<pre><code>
keysearch(Key, N, TupleIterator) -&gt; {value, Tuple} | false
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator = <a href="#type-iterator">iterator()</a></code></li><li><code>Tuple = tuple()</code></li></ul>

Searches the iterator of tuples `TupleIterator` for a tuple whose
`N`th element compares equal to `Key`. Returns `{value, Tuple}` if
such a tuple is found, otherwise `false`.

Function keyfind/3 is usually more convenient.

**See also:** [keyfind/3](#keyfind-3).

<a name="keysort-2"></a>

### keysort/2

<pre><code>
keysort(N, TupleIterator1) -&gt; TupleIterator2
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Elem = tuple()</code></li></ul>

Returns an iterator containing the sorted elements of iterator
`TupleIterator1`. Sorting is performed on the `N`th element of the
tuples. The sort is stable.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="keystore-4"></a>

### keystore/4

<pre><code>
keystore(Key, N, TupleIterator1, NewTuple) -&gt; TupleIterator2
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>NewTuple = tuple()</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem | NewTuple)</code></li></ul>

Returns a copy of `TupleIterator1` where the first occurrence of a
tuple `T` whose `N`th element compares equal to `Key` is replaced
with `NewTuple`, if there is such a tuple `T`. If there is no such
tuple `T`, a copy of `TupleIterator1` where `NewTuple` has been
appended to the end is returned.

<a name="keytake-3"></a>

### keytake/3

<pre><code>
keytake(Key, N, TupleIterator1) -&gt; {value, Tuple, TupleIterator2}
</code></pre>

<ul class="definitions"><li><code>Key = any()</code></li><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Tuple = tuple()</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Searches the iterator of tuples `TupleIterator1` for a tuple whose
`N`th element compares equal to `Key`. Returns
`{value, Tuple, TupleIterator2}` if such a tuple is found,
otherwise `false`.`TupleIterator2` is a copy of `TupleIterator1`
where the first occurrence of `Tuple` has been removed.

Evaluates `TupleIterator1` until a match is found. Iterating over
`TupleIterator2` will evaluate the same elements again. If no match
is found, infinite iterators will never return.

<a name="last-1"></a>

### last/1

<pre><code>
last(Iterator) -&gt; Elem
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the last element in `Iterator`.

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="length-1"></a>

### length/1

<pre><code>
length(Iterator) -&gt; Length
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li><li><code>Length = non_neg_integer()</code></li></ul>

Returns the length of `Iterator`, for example:

```
  > llists:length(llists:seq(1, 9)).
  9
```

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="map-2"></a>

### map/2

<pre><code>
map(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-map">map</a>(A, B)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li></ul>

Takes a function `Fun` from `A`s to `B`s, and an `Iterator1` of
`A`s and produces an `Iterator2` of `B`s by applying the function
to every element in the iterator.

<a name="mapfoldl-3"></a>

### mapfoldl/3

<pre><code>
mapfoldl(Fun, Acc0, Iterator1) -&gt; {Iterator2, AccOut}
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-mapfold">mapfold</a>(A, AccIn::Acc0 | AccOut, B, AccOut)</code></li><li><code>Acc0 = <a href="#type-accumulator">accumulator()</a></code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li></ul>

Combines the operations of `map/2` and `foldl/3` into one pass.

Example:

```
  > % Summing the elements in an iterator and double them at the same time:
  > DoubleAndSum = fun(X, Sum) -> {2*X, X+Sum} end,
  > {Mapped, Acc} = llists:mapfoldl(DoubleAndSum, 0, llists:seq(1,5)),
  > {llists:to_list(Mapped), Acc}.
  {[2,4,6,8,10],15}
```

The iterator is fully evaluated before the mapfold begins, infinite
iterators will never return.

<a name="mapfoldr-3"></a>

### mapfoldr/3

<pre><code>
mapfoldr(Fun, Acc0, Iterator1) -&gt; {Iterator2, AccOut}
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-mapfold">mapfold</a>(A, AccIn::Acc0 | AccOut, B, AccOut)</code></li><li><code>Acc0 = <a href="#type-accumulator">accumulator()</a></code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li></ul>

Combines the operations of map/2 and foldr/3 into one pass.

The iterator is fully evaluated before the mapfold begins, infinite
iterators will never return.

<a name="max-1"></a>

### max/1

<pre><code>
max(Iterator) -&gt; Elem
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the first element of `Iterator` that compares greater than
or equal to all other elements of `Iterator`.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="member-2"></a>

### member/2

<pre><code>
member(Elem, Iterator) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Elem = any()</code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns `true` if `Elem` matches some element of `Iterator`,
otherwise `false`.

Stops evaluating `Iterator` when a match is found or when
`Iterator` is empty.

<a name="merge-1"></a>

### merge/1

<pre><code>
merge(IteratorOfIterators) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IteratorOfIterators = <a href="#type-iterator">iterator</a>(<a href="#type-iterator">iterator()</a>)</code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns the sorted iterator formed by merging all the subiterators
of `IteratorOfIterators`. All subiterators must be sorted before
evaluating this function. When two elements compare equal, the
element from the subiterator with the lowest position in
`IteratorOfIterators` is picked before the other element.

The first element of each subiterator will be evaluated.

<a name="merge-2"></a>

### merge/2

<pre><code>
merge(Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(A | B)</code></li></ul>

Returns the sorted iterator formed by merging `Iterator1` and
`Iterator2`. Both `Iterator1` and `Iterator2` must be sorted before
evaluating this function. When two elements compare equal, the
element from `Iterator1` is picked before the element from
`Iterator2`.

The first element of each iterator will be evaluated.

<a name="merge-3"></a>

### merge/3

<pre><code>
merge(Fun, Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-compare">compare</a>(A, B)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(A | B)</code></li></ul>

Returns the sorted iterator formed by merging `Iterator1` and
`Iterator2`. Both `Iterator1` and `Iterator2` must be sorted
according to the ordering function `Fun` before evaluating this
function. `Fun(A, B)` is to return `true` if `A` compares less than
or equal to `B` in the ordering, otherwise `false`. When two
elements compare equal, the element from `Iterator1` is picked
before the element from `Iterator2`.

The first element of each iterator will be evaluated.

<a name="merge3-3"></a>

### merge3/3

<pre><code>
merge3(Iterator1, Iterator2, Iterator3) -&gt; Iterator4
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(C)</code></li><li><code>Iterator4 = <a href="#type-iterator">iterator</a>(A | B | C)</code></li></ul>

Returns the sorted iterator formed by merging `Iterator1`,
`Iterator2`, and `Iterator3`. All of `Iterator1`, `Iterator2`, and
`Iterator3` must be sorted before evaluating this function. When
two elements compare equal, the element from `Iterator1`, if there
is such an element, is picked before the other element, otherwise
the element from `Iterator2` is picked before the element from
`Iterator3`.

The first element of each iterator will be evaluated.

<a name="min-1"></a>

### min/1

<pre><code>
min(Iterator) -&gt; Elem
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the first element of `Iterator` that compares less than or
equal to all other elements of `Iterator`.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="next-1"></a>

### next/1

<pre><code>
next(Iterator) -&gt; LazyList
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>LazyList = <a href="#type-lazy_list">lazy_list</a>(Elem)</code></li></ul>

Demand an element from `Iterator`. Will return either an improper
list containing the next element and an iterator as a continuation,
or an empty list if iteration is complete.

Examples:

```
  > llists:next(llists:seq(1, 5)).
  [1|{iterator,#Fun<llists.1.134155648>}]
  > llists:next(llists:from_list([])).
  []
```

<a name="nth-2"></a>

### nth/2

<pre><code>
nth(N, Iterator) -&gt; Elem
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the `N`th element of `Iterator`.

Example:

```
  > lists:nth(3, [a, b, c, d, e]).
  c
```

<a name="nthtail-2"></a>

### nthtail/2

<pre><code>
nthtail(N, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the `N`th tail of `Iterator1`, that is, the subiterator of
`Iterator1` starting at `N`+1 and continuing up to the end of the
iterator.

<a name="partition-2"></a>

### partition/2

<pre><code>
partition(Pred, Iterator1) -&gt; {Satisfying, NotSatisfying}
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Satisfying = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>NotSatisfying = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Partitions `Iterator1` into two iterators, where the first iterator
contains all elements for which `Pred(Elem)` returns `true`, and
the second iterator contains all elements for which `Pred(Elem)`
returns `false`.

Examples:

```
  > {Satisfying, NotSatisfying} = llists:partition(
  >  fun(A) -> A rem 2 == 1 end,
  >  llists:seq(1, 7)),
  > {llists:to_list(Satisfying), llists:to_list(NotSatisfying)}.
  {[1,3,5,7],[2,4,6]}
  > {Satisfying, NotSatisfying} = llists:partition(
  >  fun(A) -> is_atom(A) end,
  >  llists:from_list([a,b,1,c,d,2,3,4,e])),
  > {llists:to_list(Satisfying), llists:to_list(NotSatisfying)}.
  {[a,b,c,d,e],[1,2,3,4]}
```

For a different way to partition a list, see splitwith/2.

Each result iterator will evaluate elements of the original
iterator independently. If both are evaluated, this will result in
all elements being evaluated twice.

**See also:** [splitwith/2](#splitwith-2).

<a name="prefix-2"></a>

### prefix/2

<pre><code>
prefix(Iterator1, Iterator2) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator()</a></code></li><li><code>Iterator2 = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns `true` if `Iterator1` is a prefix of `Iterator2`, otherwise `false`.

Both iterators will be evaluated until the point they diverge. If
both iterators are identical and infinite, will never return.

<a name="reverse-1"></a>

### reverse/1

<pre><code>
reverse(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(A)</code></li></ul>

Returns an iterator with the elements in `Iterator1` in reverse
order.

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="reverse-2"></a>

### reverse/2

<pre><code>
reverse(Iterator1, TailIterator) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>TailIterator = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(A | B)</code></li></ul>

Returns a list with the elements in `Iterator1` in reverse order,
with tail `TailIterator` appended.

Example:

```
  > lists:reverse([1, 2, 3, 4], [a, b, c]).
  [4,3,2,1,a,b,c]
```

The iterator `Iterator1` will be fully evaluated, infinite
iterators will never return.

<a name="search-2"></a>

### search/2

<pre><code>
search(Pred, Iterator) -&gt; {value, Value} | false
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Value)</code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

If there is a `Value` in `Iterator` such that `Pred(Value)` returns `true`,
returns `{value, Value}` for the first such `Value`, otherwise returns
`false`.

The iterator is evaluated until a match is found. If no match is
ever found, infinite iterators will never return.

<a name="seq-2"></a>

### seq/2

<pre><code>
seq(From, To) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>From = integer()</code></li><li><code>To = integer()</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(integer())</code></li></ul>

**See also:** [seq/3](#seq-3).

<a name="seq-3"></a>

### seq/3

<pre><code>
seq(From, To, Incr) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>From = integer()</code></li><li><code>To = infinity | -infinity | integer()</code></li><li><code>Incr = integer()</code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(integer())</code></li></ul>

Returns an iterator over a sequence of integers that starts with
`From` and contains the successive results of adding `Incr` to the
previous element, until `To` is reached or passed (in the latter
case, `To` is not an element of the sequence). `Incr` defaults to

1.

Failures:

- If `To < From - Incr` and `Incr > 0`.

- If `To > From - Incr` and `Incr < 0`.

- If `Incr =:= 0` and `From =/= To`.

The following equalities hold for all sequences:

```
  length(lists:seq(From, To)) =:= To - From + 1
  length(lists:seq(From, To, Incr)) =:= (To - From + Incr) div Incr
```

<a name="sort-1"></a>

### sort/1

<pre><code>
sort(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns an iterator containing the sorted elements of `Iterator1`.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="sort-2"></a>

### sort/2

<pre><code>
sort(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-compare">compare</a>(A, B)</code></li><li><code>B = A</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(A)</code></li></ul>

Returns an iterator containing the sorted elements of `Iterator1`,
according to the ordering function `Fun`. `Fun(A, B)` is to return
`true` if `A` compares less than or equal to `B` in the ordering,
otherwise `false`.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="split-2"></a>

### split/2

<pre><code>
split(N, Iterator1) -&gt; {Iterator2, Iterator3}
</code></pre>

<ul class="definitions"><li><code>N = non_neg_integer()</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Splits `Iterator1` into `Iterator2` and `Iterator3`. `Iterator2`
contains the first `N` elements and `Iterator3` the remaining
elements (the `N`th tail).

Evaluates the first `N` elements of `Iterator1` to construct
`Iterator3`.

<a name="splitwith-2"></a>

### splitwith/2

<pre><code>
splitwith(Pred, Iterator1) -&gt; {Iterator2, Iterator3}
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Partitions `Iterator1` into two iterators according to `Pred`.
`splitwith/2` behaves as if it is defined as follows:

```
  llists:splitwith(Pred, Iterator) ->
      {llists:takewhile(Pred, Iterator),
       llists:dropwhile(Pred, Iterator)}.
```

Examples:

```
  > {Before, After} = llists:splitwith(fun(A) -> A rem 2 == 1 end, llists:seq(1, 7)),
  > {llists:to_list(Before), llists:to_list(After)}.
  {[1],[2,3,4,5,6,7]}
  > {Before, After} = lists:splitwith(fun(A) -> is_atom(A) end, [a,b,1,c,d,2,3,4,e]),
  > {llists:to_list(Before), llists:to_list(After)}.
  {[a,b],[1,c,d,2,3,4,e]}
```

For a different way to partition an iterator, see partition/2.

Evaluates the elements of `Iterator` for which `Pred(Elem)` returns
`false`. If `Pred` never returns `false`, infinite iterators will
not return.

**See also:** [partition/2](#partition-2).

<a name="sublist-2"></a>

### sublist/2

<pre><code>
sublist(Iterator1, Len) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Len = non_neg_integer()</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

**See also:** [sublist/3](#sublist-3).

<a name="sublist-3"></a>

### sublist/3

<pre><code>
sublist(Iterator1, Start, Len) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Start = pos_integer()</code></li><li><code>Len = non_neg_integer()</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the portion of `Iterator1` starting at `Start` and with
(maximum) `Len` elements. `Start` defaults to 1. It is not an error
for `Start+Len` to exceed the length of the iterator, in that case
the whole iterator is returned.

<a name="subtract-2"></a>

### subtract/2

<pre><code>
subtract(Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator()</a></code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns a new iterator `Iterator3` that is a copy of `Iterator1`,
subjected to the following procedure: for each element in
`Iterator2`, its first occurrence in `Iterator1` is deleted.

Example:

```
  > lists:subtract("123212", "212").
  "312".
```

`Iterator2` is fully evaluated, infinite iterators will never return.

<a name="suffix-2"></a>

### suffix/2

<pre><code>
suffix(Iterator1, Iterator2) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator()</a></code></li><li><code>Iterator2 = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns `true` if `Iterator1` is a suffix of `Iterator2`, otherwise
`false`.

Both `Iterator1` and `Iterator2` are fully evaluated, infinite
iterators will never return.

<a name="sum-1"></a>

### sum/1

<pre><code>
sum(Iterator) -&gt; Sum
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Sum = Elem</code></li><li><code>Elem = number()</code></li></ul>

Returns the sum of the elements in `Iterator`.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="takewhile-2"></a>

### takewhile/2

<pre><code>
takewhile(Pred, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Pred = <a href="#type-predicate">predicate</a>(Elem)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Takes elements `Elem` from `Iterator1` while `Pred(Elem)` returns
true, that is, the function returns the longest prefix of the
iterator for which all elements satisfy the predicate.

<a name="tl-1"></a>

### tl/1

<pre><code>
tl(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns the tail of `Iterator1`, that is, the iterator minus the
first element, for example:

```
  > llists:to_list(
  >  llists:tl(
  >   llists:from_list([geesties, guilies, beasties]))).
  [guilies, beasties]
```

Failure: `badarg` if `Iterator1` is empty.

<a name="to_list-1"></a>

### to_list/1

<pre><code>
to_list(Iterator) -&gt; List
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>List = [Elem]</code></li></ul>

Fully evaluate `Iterator` and return a list containing all elements
produced. Infinite iterators will never return.

<a name="to_map-1"></a>

### to_map/1

<pre><code>
to_map(Iterator) -&gt; Map
</code></pre>

<ul class="definitions"><li><code>Iterator = <a href="#type-iterator">iterator</a>({Key, Value})</code></li><li><code>Key = any()</code></li><li><code>Value = any()</code></li><li><code>Map = <a href="maps.md#type-map">maps:map</a>(Key, Value)</code></li></ul>

Fully evaluate an `Iterator` of `{Key, Value}` tuples and return a
map containing all pairs produced. Infinite iterators will never
return.

If duplicate `Key`s are present in the iterator it is undefined
which will appear in the final map.

<a name="ukeymerge-3"></a>

### ukeymerge/3

<pre><code>
ukeymerge(N, TupleIterator1, TupleIterator2) -&gt; TupleIterator3
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem1)</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem2)</code></li><li><code>TupleIterator3 = <a href="#type-iterator">iterator</a>(Elem1 | Elem2)</code></li><li><code>Elem1 = tuple()</code></li><li><code>Elem2 = tuple()</code></li></ul>

Returns the sorted iterator formed by merging `TupleIterator1` and
`TupleIterator2`. The merge is performed on the `N`th element of each
tuple. Both `TupleIterator1` and `TupleIterator2` must be key-sorted without
duplicates before evaluating this function. When two tuples compare
equal, the tuple from `TupleIterator1` is picked and the one from
`TupleIterator2` is deleted.

The first element of each iterator will be evaluated.

<a name="ukeysort-2"></a>

### ukeysort/2

<pre><code>
ukeysort(N, TupleIterator1) -&gt; TupleIterator2
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>TupleIterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>TupleIterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Elem = tuple()</code></li></ul>

Returns a iterator containing the sorted elements of iterator
`TupleIterator1` where all except the first tuple of the tuples
comparing equal have been deleted. Sorting is performed on the
`N`th element of the tuples.

The iterator is fully evaluated, infinite iterators will never
return.

<a name="umerge-1"></a>

### umerge/1

<pre><code>
umerge(IteratorOfIterators) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>IteratorOfIterators = <a href="#type-iterator">iterator</a>(<a href="#type-iterator">iterator()</a>)</code></li><li><code>Iterator = <a href="#type-iterator">iterator()</a></code></li></ul>

Returns the sorted iterator formed by merging all the subiterators of
`IteratorOfIterators`. All subiterators must be sorted and contain no duplicates
before evaluating this function. When two elements compare equal,
the element from the subiterator with the lowest position in
`IteratorOfIterators` is picked and the other is deleted.

The first element of each subiterator will be evaluated.

<a name="umerge-2"></a>

### umerge/2

<pre><code>
umerge(Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(A | B)</code></li></ul>

Returns the sorted iterator formed by merging `Iterator1` and
`Iterator2`. Both `Iterator1` and `Iterator2` must be sorted and
contain no duplicates before evaluating this function. When two
elements compare equal, the element from `Iterator1` is picked and
the one from `Iterator2` is deleted.

The first element of each iterator will be evaluated.

<a name="umerge-3"></a>

### umerge/3

<pre><code>
umerge(Fun, Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-compare">compare</a>(A, B)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(A | B)</code></li></ul>

Returns the sorted iterator formed by merging `Iterator1` and
`Iterator2`. Both `Iterator1` and `Iterator2` must be sorted
according to the ordering function `Fun` and contain no duplicates
before evaluating this function. `Fun(A, B)` is to return `true` if
`A` compares less than or equal to `B` in the ordering, otherwise
`false`. When two elements compare equal, the element from
`Iterator1` is picked and the one from `Iterator2` is deleted.

The first element of each iterator will be evaluated.

<a name="umerge3-3"></a>

### umerge3/3

<pre><code>
umerge3(Iterator1, Iterator2, Iterator3) -&gt; Iterator4
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(C)</code></li><li><code>Iterator4 = <a href="#type-iterator">iterator</a>(A | B | C)</code></li></ul>

Returns the sorted iterator formed by merging `Iterator1`,
`Iterator2`, and `Iterator3`. All of `Iterator1`, `Iterator2`, and
`Iterator3` must be sorted and contain no duplicates before
evaluating this function. When two elements compare equal, the
element from `Iterator1` is picked if there is such an element,
otherwise the element from `Iterator2` is picked, and the other is
deleted.

The first element of each iterator will be evaluated.

<a name="unfold-2"></a>

### unfold/2

<pre><code>
unfold(Fun, Acc0) -&gt; Iterator
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-unfold">unfold</a>(Elem, AccIn::Acc0 | AccOut, AccOut)</code></li><li><code>Acc0 = <a href="#type-accumulator">accumulator()</a></code></li><li><code>Iterator = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Construct a new iterator from a `Fun(AccIn)` function and an
initial accumulator value `Acc0`. When an element is demanded of
the iterator, `Fun` will be invoked with the current accumulator to
produce a value. `Fun` is expected to return a tuple of
`{Elem, AccOut}`: the element to produce and the new accumulator
value. If iteration is complete, `Fun` should return `none`.

<a name="unzip-1"></a>

### unzip/1

<pre><code>
unzip(Iterator1) -&gt; {Iterator2, Iterator3}
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>({A, B})</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(B)</code></li></ul>

"Unzips" a iterator of two-tuples into two iterators, where the
first iterator contains the first element of each tuple, and the
second iterator contains the second element of each tuple.

<a name="unzip3-1"></a>

### unzip3/1

<pre><code>
unzip3(Iterator1) -&gt; {Iterator2, Iterator3, Iterator4}
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>({A, B, C})</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator4 = <a href="#type-iterator">iterator</a>(C)</code></li></ul>

"Unzips" a iterator of three-tuples into three iterators, where the first
iterator contains the first element of each tuple, the second iterator
contains the second element of each tuple, and the third iterator
contains the third element of each tuple.

<a name="usort-1"></a>

### usort/1

<pre><code>
usort(Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(Elem)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Elem)</code></li></ul>

Returns a iterator containing the sorted elements of `Iterator1`
where all except the first element of the elements comparing equal
have been deleted.

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="usort-2"></a>

### usort/2

<pre><code>
usort(Fun, Iterator1) -&gt; Iterator2
</code></pre>

<ul class="definitions"><li><code>Fun = <a href="#type-compare">compare</a>(A, B)</code></li><li><code>B = A</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(A)</code></li></ul>

Returns a iterator containing the sorted elements of `Iterator1` where all
except the first element of the elements comparing equal according
to the ordering function `Fun` have been deleted. `Fun(A, B)` is to
return `true` if `A` compares less than or equal to `B` in the ordering,
otherwise `false`.

The iterator will be fully evaluated, infinite iterators will never
return.

<a name="zip-2"></a>

### zip/2

<pre><code>
zip(Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>({A, B})</code></li></ul>

"Zips" two iterators of equal length into one iterator of
two-tuples, where the first element of each tuple is taken from the
first iterator and the second element is taken from the
corresponding element in the second iterator.

<a name="zip3-3"></a>

### zip3/3

<pre><code>
zip3(Iterator1, Iterator2, Iterator3) -&gt; Iterator4
</code></pre>

<ul class="definitions"><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(C)</code></li><li><code>Iterator4 = <a href="#type-iterator">iterator</a>({A, B, C})</code></li></ul>

"Zips" three iterators of equal length into one iterator of
three-tuples, where the first element of each tuple is taken from
the first iterator, the second element is taken from the
corresponding element in the second iterator, and the third element
is taken from the corresponding element in the third iterator.

<a name="zipwith-3"></a>

### zipwith/3

<pre><code>
zipwith(Combine, Iterator1, Iterator2) -&gt; Iterator3
</code></pre>

<ul class="definitions"><li><code>Combine = <a href="#type-combine">combine</a>(X, Y, Out)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(X)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(Y)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(Out)</code></li></ul>

Combines the elements of two iterators of equal length into one iterator.
For each pair `X, Y` of iterator elements from the two iterators, the element
in the result iterator is `Combine(X, Y)`.

`llists:zipwith(fun(X, Y) -> {X, Y} end, Iterator1, Iterator2)` is
equivalent to `llists:zip(Iterator1, Iterator2)`.

Example:

```
  > llists:to_list(
  >  llists:zipwith(fun(X, Y) -> X + Y end, llists:seq(1, 3), llist:seq(4, 6))).
  [5,7,9]
```

<a name="zipwith3-4"></a>

### zipwith3/4

<pre><code>
zipwith3(Combine, Iterator1, Iterator2, Iterator3) -&gt; Iterator4
</code></pre>

<ul class="definitions"><li><code>Combine = <a href="#type-combine3">combine3</a>(A, B, C, Out)</code></li><li><code>Iterator1 = <a href="#type-iterator">iterator</a>(A)</code></li><li><code>Iterator2 = <a href="#type-iterator">iterator</a>(B)</code></li><li><code>Iterator3 = <a href="#type-iterator">iterator</a>(C)</code></li><li><code>Iterator4 = <a href="#type-iterator">iterator</a>(Out)</code></li></ul>

Combines the elements of three iterators of equal length into one
iterator. For each triple `X, Y, Z` of iterator elements from the
three iterators, the element in the result iterator is
`Combine(X, Y, Z)`.

`zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, Iterator1, Iterator2, Iterator3)`
is equivalent to `zip3(Iterator1, Iterator2, Iterator3)`.

Examples:

```
  > llists:to_list(
  >  llists:zipwith3(
  >   fun(X, Y, Z) -> X + Y + Z end,
  >   llists:seq(1, 3),
  >   llists:seq(4, 6),
  >   llists:seq(7, 9))).
  [12,15,18]
  > llists:to_list(
  >  llists:zipwith3(
  >   fun(X, Y, Z) -> [X, Y, Z] end,
  >   llists:from_list([a,b,c]),
  >   llists:from_list([x,y,z]),
  >   llists:seq(1, 3))).
  [[a,x,1],[b,y,2],[c,z,3]]
```
