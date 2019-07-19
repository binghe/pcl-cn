# Sequence Predicates（序列谓词）

Four other handy functions are **EVERY**, **SOME**, **NOTANY**, and **NOTEVERY**,
which iterate over sequences testing a boolean predicate. The first
argument to all these functions is the predicate, and the remaining
arguments are sequences. The predicate should take as many arguments
as the number of sequences passed. The elements of the sequences are
passed to the predicate--one element from each sequence--until one of
the sequences runs out of elements or the overall termination test is
met: **EVERY** terminates, returning false, as soon as the predicate
fails. If the predicate is always satisfied, it returns true. **SOME**
returns the first non-**NIL** value returned by the predicate or returns
false if the predicate is never satisfied. **NOTANY** returns false as
soon as the predicate is satisfied or true if it never is. And
**NOTEVERY** returns true as soon as the predicate fails or false if the
predicate is always satisfied. Here are some examples of testing just
one sequence:

另外四个常见的函数是 **EVERY**、**SOME**、**NOTANY** 和
**NOTEVERY**，它们在序列上迭代并测试一个布尔谓词。所有这些函数的第一参数是一个谓词，其余的参数都是序列。这个谓词应当接受与所传递序列相同数量的参数。序列的元素被传递给该谓词，每个序列中各取出一个元素，直到某个序列用完所有的元素或是整体终止测试条件被满足：**EVERY**
在谓词失败时返回假。如果谓词总被满足，它返回真。**SOME**
返回由谓词所返回的第一个非NIL值，或者在谓词永远得不到满足时返回假。**NOTANY**
将在谓词满足时返回假，或者在从未满足时返回真。而
**NOTEVERY**
在谓词失败时返回真，或是在谓词总是满足时返回假。下面是一些仅测试在一个序列上的例子：

```lisp
(every #'evenp #(1 2 3 4 5))    ==> NIL
(some #'evenp #(1 2 3 4 5))     ==> T
(notany #'evenp #(1 2 3 4 5))   ==> NIL
(notevery #'evenp #(1 2 3 4 5)) ==> T
```

These calls compare elements of two sequences pairwise:

下面的调用比较成对的两个序列中的元素：

```lisp
(every #'> #(1 2 3 4) #(5 4 3 2))    ==> NIL
(some #'> #(1 2 3 4) #(5 4 3 2))     ==> T
(notany #'> #(1 2 3 4) #(5 4 3 2))   ==> NIL
(notevery #'> #(1 2 3 4) #(5 4 3 2)) ==> T
```
