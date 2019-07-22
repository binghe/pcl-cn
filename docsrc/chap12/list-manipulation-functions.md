# List-Manipulation Functions（列表处理函数）

With that background out of the way, you're ready to look at the
library of functions Common Lisp provides for manipulating lists.

有了前面这些背景知识，现在就可以开始学习 Common Lisp
为处理列表而提供的函数库了。

You've already seen the basic functions for getting at the elements of
a list: **FIRST** and **REST**. Although you can get at any element of a list
by combining enough calls to **REST** (to move down the list) with a **FIRST**
(to extract the element), that can be a bit tedious. So Common Lisp
provides functions named for the other ordinals from **SECOND** to **TENTH**
that return the appropriate element. More generally, the function **NTH**
takes two arguments, an index and a list, and returns the nth
(zero-based) element of the list. Similarly, **NTHCDR** takes an index and
a list and returns the result of calling **CDR** n times. (Thus,
`(nthcdr 0 ...)` simply returns the original list, and `(nthcdr 1 ...)` is
equivalent to **REST**.) Note, however, that none of these functions is
any more efficient, in terms of work done by the computer, than the
equivalent combinations of **FIRST**s and **REST**s--there's no way to get to
the nth element of a list without following n **CDR** references.

前面已经介绍了获取列表中元素的基本函数：**FIRST** 和
**REST**。尽管可以通过将足够多的 **REST** 调用（用于深入列表）和一个
**FIRST** 调用（用于抽取元素）组合起来以获得一个列表中的任意元素，但这样可能有点冗长。因此，Common
Lisp 提供了以从 **SECOND** 到 **TENTH**
的由其他序数命名的函数来返回相应的元素。而函数 **NTH**
则更为普遍，它接受两个参数，一个索引和一个列表，并返回列表中第 n
个（从 0 开始）元素。类似地，**NTHCDR** 接受一个索引和一个列表并返回 n
次调用 **CDR** 的结果。
（这样，`(nthcdr 0 ...)` 简单地返回最初的列表，而 `(nthcdr 1 ...)`
等价于 **REST**。）但要注意的是，就计算机完成的工作而言，这些函数都不会比等价的
**FIRST** 和 **REST**
组合更高效，因位无法在没有跟随 n 个 **CDR**
引用的情况下得到一个列表的第 n 个元素。

The 28 composite **CAR**/**CDR** functions are another family of functions you
may see used from time to time. Each function is named by placing a
sequence of up to four `A`s and `D`s between a `C` and `R`, with each A
representing a call to **CAR** and each `D` a call to **CDR**. Thus:

另一个不时会用到的函数家族是 28 个复合 **CAR**/**CDR** 函数。每个函数都是通过将由最多四个 `A` 和 `D`
组成的序列放在 `C` 和 `R` 之间来命名的，其中每个 `A` 代表对 **CAR**
的调用而每个 `D` 代表对 **CDR** 的调用。因此我们可以得到：

```lisp
(caar list) === (car (car list))
(cadr list) === (car (cdr list))
(cadadr list) === (car (cdr (car (cdr list))))
```

Note, however, that many of these functions make sense only when
applied to lists that contain other lists. For instance, **CAAR** extracts
the **CAR** of the **CAR** of the list it's given; thus, the list it's passed
must contain another list as its first element. In other words, these
are really functions on trees rather than lists:

但要注意，这其中许多函数仅当应用于含有其他列表的列表时才有意义。例如，**CAAR**
抽取出给定列表的 **CAR** 的
**CAR**，因此传递给它的列表必须含有另一个列表，并将该列表用作其第一个元素。换句话说，这些函数其实是用于树而不是列表的：

```lisp
(caar (list 1 2 3))                  ==> error
(caar (list (list 1 2) 3))           ==> 1
(cadr (list (list 1 2) (list 3 4)))  ==> (3 4)
(caadr (list (list 1 2) (list 3 4))) ==> 3
```

These functions aren't used as often now as in the old days. And even
the most die-hard old-school Lisp hackers tend to avoid the longer
combinations. However, they're used quite a bit in older Lisp code, so
it's worth at least understanding how they work.

现在这些函数不像以前那样常用了。并且即便是最顽固的守旧 Lisp
黑客也倾向于避免使用过长的组合。尽管如此，它们还是被用在很多古老的 Lisp
代码上，因此至少应当去理解它们的工作方式。

The **FIRST-TENTH** and **CAR**, **CADR**, and so on, functions can also be used
as **SETF**able places if you're using lists nonfunctionally.

如果你正在非函数式地使用列表，这些 **FIRST-TENTH** 和 **CAR**、**CADR**
等函数也可被用作 **SETF** 的位置。

Table 12-1 summarizes some other list functions that I won't cover in detail.

表 12-1 总结了其他一些我不会详细介绍的列表函数。

> Table 12-1. Other List Functions

| Function	| Description  |
| :-------- | :----------- |
| **LAST** | Returns the last cons cell in a list. With an integer, argument returns the last n cons cells. |
| **BUTLAST** | Returns a copy of the list, excluding the last cons cell. With an integer argument, excludes the last n cells. |
| **NBUTLAST** | The recycling version of **BUTLAST**; may modify and return the argument list but has no reliable side effects. |
| **LDIFF**	| Returns a copy of a list up to a given cons cell.	|
| **TAILP** | Returns true if a given object is a cons cell that's part of the structure of a list.	|
| `LIST*`	| Builds a list to hold all but the last of its arguments and then makes the last argument the CDR of the last cell in the list. In other words, a cross between **LIST** and **APPEND**.	|
| **MAKE-LIST**	| Builds an n item list. The initial elements of the list are **NIL** or the value specified with the :initial-element keyword argument.	|
| **REVAPPEND**	| Combination of **REVERSE** and **APPEND**; reverses first argument as with **REVERSE** and then appends the second argument.	|
| **NRECONC** | Recycling version of **REVAPPEND**; reverses first argument as if by **NREVERSE** and then appends the second argument. No reliable side effects.	|
| **CONSP**	| Predicate to test whether an object is a cons cell.	|
| **ATOM**	| Predicate to test whether an object is not a cons cell.	|
| **LISTP**	| Predicate to test whether an object is either a cons cell or **NIL**.	|
| **NULL**	| Predicate to test whether an object is **NIL**. Functionally equivalent to **NOT** but stylistically preferable when testing for an empty list as opposed to boolean false.	|

