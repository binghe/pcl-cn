# Sorting and Merging（排序与合并）

The functions **SORT** and **STABLE-SORT** provide two ways of sorting
a sequence. They both take a sequence and a two-argument predicate and
return a sorted version of the sequence.

函数 **SORT** 和 **STABLE-SORT**
提供了两种序列排序方式。它们都接受一个序列和一个由两个实参组成的谓词并返回该序列排序后的版本。

```lisp
(sort (vector "foo" "bar" "baz") #'string<) ==> #("bar" "baz" "foo")
```

The difference is that **STABLE-SORT** is guaranteed to not reorder any
elements considered equivalent by the predicate while **SORT** guarantees
only that the result is sorted and may reorder equivalent elements.

它们的区别在于，**STABLE-SORT**
可以保证不会重排任何被该谓词视为等价的元素，而 **SORT**
只保证结果是排序了的并可能重排等价元素。

Both these functions are examples of what are called destructive
functions. Destructive functions are allowed--typically for reasons of
efficiency--to modify their arguments in more or less arbitrary
ways. This has two implications: one, you should always do something
with the return value of these functions (such as assign it to a
variable or pass it to another function), and, two, unless you're done
with the object you're passing to the destructive function, you should
pass a copy instead. I'll say more about destructive functions in the
next chapter.

这两个函数都是所谓的破坏性（destructive）函数。通常出于效率的原因，破坏性函数都会或多或少地修改它们的参数。这有两层含义：第一，你应该总是对这些函数的返回值做一些事情（比如给它赋值一个变量或将它传递给另一个函数）；第二，除非你不再需要你传给破坏性函数的那个对象，否则你应该代替地传递一个副本。下一章里将会讨论更多有关破坏性函数的内容。

Typically you won't care about the unsorted version of a sequence
after you've sorted it, so it makes sense to allow **SORT** and
**STABLE-SORT** to destroy the sequence in the course of sorting it. But
it does mean you need to remember to write the following:

通常在排序以后你不会再关心那个序列的未排序版本，因此在排序的过程中，允许
**SORT** 和 **STABLE-SORT** 破坏序列是合理的。但这意味着需要记得像这样来写：

```lisp
(setf my-sequence (sort my-sequence #'string<))
```

rather than just this:

而不是这样：

```lisp
(sort my-sequence #'string<)
```

Both these functions also take a keyword argument, `:key`, which, like
the `:key` argument in other sequence functions, should be a function
and will be used to extract the values to be passed to the sorting
predicate in the place of the actual elements. The extracted keys are
used only to determine the ordering of elements; the sequence returned
will contain the actual elements of the argument sequence.

这两个函数也接受关键字参数 `:key`，它和其他序列函数的 `:key`
参数一样，应当是一个将被用来从序列元素中抽取出传给排序谓词的值的函数。被抽取出的关键字仅用于确定元素顺序，返回的序列将含有参数序列的实际元素。

The **MERGE** function takes two sequences and a predicate and returns a
sequence produced by merging the two sequences, according to the
predicate. It's related to the two sorting functions in that if each
sequence is already sorted by the same predicate, then the sequence
returned by **MERGE** will also be sorted. Like the sorting functions,
**MERGE** takes a `:key` argument. Like **CONCATENATE**, and for the same
reason, the first argument to **MERGE** must be a type descriptor
specifying the type of sequence to produce.

函数 **MERGE**
接受两个序列和一个谓词并返回按照该谓词合并两个序列所产生的序列。它和两个排序函数之间的关系在于，如果每个序列已经被同样的谓词排序过了，那么由
**MERGE** 返回的序列也将是有序的。和排序函数一样，**MERGE** 也接受一个
`:key` 参数。并且出于同样原因，和 **CONCATENATE**
一样，**MERGE** 的第一个参数必须是用来指定所生成序列类型的类型描述符。

```lisp
(merge 'vector #(1 3 5) #(2 4 6) #'<) ==> #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<)   ==> (1 2 3 4 5 6)
```
