# Combining Recycling with Shared Structure（组合回收性函数和共享结构）

Although you can use recycling functions whenever the arguments to the
recycling function won't be used after the function call, it's worth
noting that each recycling function is a loaded gun pointed footward:
if you accidentally use a recycling function on an argument that is
used later, you're liable to lose some toes.

尽管可以在函数实参在函数调用之后不会被使用的情况下使用回收性函数，但值得注意的是，每个回收性函数都是一把指向脚面的装了子弹的枪：如果不小心将一个回收性函数用在了以后会用到的参数上，你肯定会失去一些脚趾。

To make matters worse, shared structure and recycling functions tend
to work at cross-purposes. Nondestructive list functions return lists
that share structure under the assumption that cons cells are never
modified, but recycling functions work by violating that
assumption. Or, put another way, sharing structure is based on the
premise that you don't care exactly what cons cells make up a list
while using recycling functions requires that you know exactly what
cons cells are referenced from where.

使事情变得更糟的是，共享结构和回收性函数会用于不同的目的。非破坏性列表函数在点对单元永远不会被修改的假设下返回带有共享结构的列表，但是回收性函数却通过违反这一假设得以正常工作。或者换另一种说法，使用共享结构是基于不在乎究竟由哪些点对单元构成列表这一前提的，而使用回收性函数则要求精确地知道哪些点对单元会在哪里被引用到。

In practice, recycling functions tend to be used in a few idiomatic
ways. By far the most common recycling idiom is to build up a list to
be returned from a function by "consing" onto the front of a list,
usually by PUSHing elements onto a list stored in a local variable and
then returning the result of **NREVERSE**ing it.

在实践中，回收性函数会有一些习惯用法。其中最常见的一种是构造一个列表，它是由一个在列表前端不断做点对分配操作的函数返回，通常是将元素
**PUSH** 进一个保存在局部变量中的列表里，然后返回对其 **NREVERSE** 的结果。

This is an efficient way to build a list because each **PUSH** has to
create only one cons cell and modify a local variable and the **NREVERSE**
just has to zip down the list reassigning the **CDR**s. Because the list
is created entirely within the function, there's no danger any code
outside the function has a reference to any of its cons cells. Here's
a function that uses this idiom to build a list of the first n
numbers, starting at zero:

这是一种构造列表的有效方式，因为每次 **PUSH**
都只创建一个点对单元并修改一个局部变量，而 **NREVERSE**
只需穿过列表并重新赋值每个元素的
**CDR**。由于列表完全是在函数之内创建，所以完全不存在任何函数之外的代码会引用列表的任何点对单元的风险。下面是一个函数使用该习惯用法来构造一个由从
0 开始的前 n 个数字所组成的列表：

```lisp
(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(upto 10) ==> (0 1 2 3 4 5 6 7 8 9)
```

The next most common recycling idiom is to immediately reassign the
value returned by the recycling function back to the place containing
the potentially recycled value. For instance, you'll often see
expressions like the following, using **DELETE**, the recycling version of
**REMOVE**:

还有一个最常见的回收性习惯用法， 是将回收性函数的返回值立即重新赋值到含有可能会被回收的值的位置上。例如，你将经常看到像下面这样的表达式，它使用了
**DELETE**，即 **REMOVE** 的回收性版本：

```lisp
(setf foo (delete nil foo))
```

This sets the value of `foo` to its old value except with all the **NIL**s
removed. However, even this idiom must be used with some care--if foo
shares structure with lists referenced elsewhere, using **DELETE** instead
of **REMOVE** can destroy the structure of those other lists. For example,
consider the two lists *list-2* and *list-3* from earlier that share
their last two cons cells.

这将 `foo` 的值设置到了它的旧值上，只是所有的 **NIL**
都被移除了。但即便是这种习惯用法，你在使用时也需小心一些。如果 `foo`
和在其他位置上引用的列表共享了一些结构，那么使用 **DELETE** 来代替
**REMOVE**
可能会破坏其他那些列表的结构。例如早先那两个共享了它们最后两个点对单元的列表
`*list-2*` 和 `*list-3*`：

```lisp
*list-2* ==> (0 4)
*list-3* ==> (1 2 0 4)
```

You can delete 4 from `*list-3*` like this:

你可以像下面这样将 4 从 `*list-3*` 中删除：

```lisp
(setf *list-3* (delete 4 *list-3*)) ==> (1 2 0)
```

However, **DELETE** will likely perform the necessary deletion by setting
the **CDR** of the third cons cell to **NIL**, disconnecting the fourth cons
cell, the one holding the 4, from the list. Because the third cons
cell of `*list-3*` is also the first cons cell in `*list-2*`, the
following modifies `*list-2*` as well:

不过，**DELETE** 将很可能进行必要的删除，通过将第三个点对单元的
**CDR** 设置为 **NIL**，从而从列表中断开了第四个保存了数字
4 的点对单元。由于 `*list-3*` 的第三个点对单元同时也是 `*list-2*`
的第一个点对单元，所以上述操作也改变了 `*list-2*`：

```lisp
*list-2* ==> (0)
```

If you had used **REMOVE** instead of **DELETE**, it would've built a list
containing the values 1, 2, and 0, creating new cons cells as
necessary rather than modifying any of the cons cells in `*list-3*`. In
that case, `*list-2*` wouldn't have been affected.

如果使用 **REMOVE** 来代替 **DELETE**，它将会构造一个含有值 1、2 和 0
的列表，在必要时创建新的点对单元而不会修改 `*list-3*`
中的任何点对单元。在这种情况下，`*list-2*` 将不会受到影响。

The **PUSH**/**NREVERSE** and **SETF**/**DELETE** idioms probably account for 80
percent of the uses of recycling functions. Other uses are possible
but require keeping careful track of which functions return shared
structure and which do not.

**PUSH**/**NREVERSE** 和 **SETF**/**DELETE**
的习惯用法很可能占据了 80%
的回收性函数使用。其他的使用是可能的，但需要小心地跟踪哪些函数返回共享的结构而哪些没有。

In general, when manipulating lists, it's best to write your own code
in a functional style--your functions should depend only on the
contents of their list arguments and shouldn't modify them. Following
that rule will, of course, rule out using any destructive functions,
recycling or otherwise. Once you have your code working, if profiling
shows you need to optimize, you can replace nondestructive list
operations with their recycling counterparts but only if you're
certain the argument lists aren't referenced from anywhere else.

总之，当操作列表时，最好是以函数式风格来编写自己的代码——函数应当只依赖于它们的列表实参的内容而不应该修改它们。当然，按照这样的规则将会排除对任何破坏性函数的使用，无论是回收性的还是其他。一旦运行了代码，如果性能评估显示需要进行优化，你可以将非破坏性列表操作替换成相应的回收性操作，但只有当你确定其他任何位置不会引用实参列表时才可以这样做。

One last gotcha to watch out for is that the sorting functions **SORT**,
**STABLE-SORT**, and **MERGE** mentioned in Chapter 11 are also recycling
functions when applied to lists. However, these functions don't have
nondestructive counterparts, so if you need to sort a list without
destroying it, you need to pass the sorting function a copy made with
**COPY-LIST**. In either case you need to be sure to save the result of
the sorting function because the original argument is likely to be in
tatters. For instance:

最后需要注意的是，当第 11 章里提到的排序函数 **SORT**、**STABLE-SORT**
和 **MERGE** 当应用于列表时，它们也是回收性函数。 不过，这些函数却并没有非破坏性的同伴，因此当需要对列表排序而又不破坏它时，你需要传给排序函数一个由
**COPY-LIST**
生成的列表副本。无论哪种情况，你都需要确保可以保存排序函数的结果，因为原先的实参很可能已经一团糟了。例如：

```lisp
CL-USER> (defparameter *list* (list 4 3 2 1))
*LIST*
CL-USER> (sort *list* #'<)
(1 2 3 4)                      ; looks good
CL-USER> *list*
(4)                            ; whoops!
```
