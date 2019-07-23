# Sets（集合）

Sets can also be implemented in terms of cons cells. In fact, you can
treat any list as a set--Common Lisp provides several functions for
performing set-theoretic operations on lists. However, you should bear
in mind that because of the way lists are structured, these operations
get less and less efficient the bigger the sets get.

集合也可以用点对单元来实现。事实上，你可以将任何列表都看作是集合──Common
Lisp
提供的几个函数可用于对列表进行集合论意义上的操作。但你应当在头脑中牢记，由于列表结构的组织方式，当集合变得更大时，这些操作将会越来越低效。

That said, using the built-in set functions makes it easy to write
set-manipulation code. And for small sets they may well be more
efficient than the alternatives. If profiling shows you that these
functions are a performance bottleneck in your code, you can always
replace the lists with sets built on top of hash tables or bit
vectors.

也就是说，使用内置的集合函数可以轻松地写出集合操作的代码，并且对于小型的集合而言，它们可能会比其他替代实现更为高效。如果性能评估显示这些函数成为代码的性能瓶颈，那么你也总能将列表替换成构建在哈希表或位向量之上的集合。

To build up a set, you can use the function **ADJOIN**. **ADJOIN** takes an
item and a list representing a set and returns a list representing the
set containing the item and all the items in the original set. To
determine whether the item is present, it must scan the list; if the
item isn't found, **ADJOIN** creates a new cons cell holding the item and
pointing to the original list and returns it. Otherwise, it returns
the original list.

可以使用函数 **ADJOIN** 来构造集合。**ADJOIN**
接受一个项和一个代表集合的列表并返回另一个代表集合的列表，其中含有该项和原先集合中的所有项。为了检测该项是否存在，它必须扫描该列表。如果该项没有被找到，那么
**ADJOIN**
就会创建一个保存该项的新点对单元，并让其指向原先的列表并返回它。否则，它返回原先的列表。

**ADJOIN** also takes `:key` and `:test` keyword arguments, which are used
when determining whether the item is present in the original
list. Like **CONS**, **ADJOIN** has no effect on the original list--if you
want to modify a particular list, you need to assign the value
returned by **ADJOIN** to the place where the list came from. The modify
macro **PUSHNEW** does this for you automatically.

**ADJOIN** 也接受 `:key` 和 `:test`
关键字参数，它们被用于检测该项是否存在于原先的列表中。和
**CONS** 一样，**ADJOIN**
不会影响原先的列表──如果打算修改一个特定的列表，则需要将 **ADJOIN**
返回的值赋值到该列表所来自的位置上。**PUSHNEW** 修改宏可以自动做到这点。

```lisp
CL-USER> (defparameter *set* ())
*SET*
CL-USER> (adjoin 1 *set*)
(1)
CL-USER> *set*
NIL
CL-USER> (setf *set* (adjoin 1 *set*))
(1)
CL-USER> (pushnew 2 *set*)
(2 1)
CL-USER> *set*
(2 1)
CL-USER> (pushnew 2 *set*)
(2 1)
```

You can test whether a given item is in a set with **MEMBER** and the
related functions **MEMBER-IF** and **MEMBER-IF-NOT**. These functions are
similar to the sequence functions **FIND**, **FIND-IF**, and **FIND-IF-NOT**
except they can be used only with lists. And instead of returning the
item when it's present, they return the cons cell containing the
item--in other words, the sublist starting with the desired item. When
the desired item isn't present in the list, all three functions return
**NIL**.

你可以使用 **MEMBER** 和相关的函数 **MEMBER-IF** 以及 **MEMBER-IF-NOT**
来测试一个给定项是否在一个集合中。这些函数与序列函数 **FIND**、**FIND-IF**
以及 **FIND-IF-NOT**
相似，不过它们只能用于列表。当指定项存在时，它们并不返回该项，而是返回含有该项的那个点对单元，即以指定项开始的子列表。当指定项不在列表中时，所有三个函数均返回 **NIL**。

The remaining set-theoretic functions provide bulk operations:
**INTERSECTION**, **UNION**, **SET-DIFFERENCE**, and **SET-EXCLUSIVE-OR**. Each of
these functions takes two lists and `:key` and `:test` keyword arguments
and returns a new list representing the set resulting from performing
the appropriate set-theoretic operation on the two lists: **INTERSECTION**
returns a list containing all the elements found in both
arguments. **UNION** returns a list containing one instance of each unique
element from the two arguments. **SET-DIFFERENCE** returns a list
containing all the elements from the first argument that don't appear
in the second argument. And **SET-EXCLUSIVE-OR** returns a list containing
those elements appearing in only one or the other of the two argument
lists but not in both. Each of these functions also has a recycling
counterpart whose name is the same except with an `N` prefix.

其余的集合论函数提供了批量操作：**INTERSECTION**、**UNION**、**SET-DIFFERENCE**
以及 **SET-EXCLUSIVE-OR**。这些函数中的每一个都接受两个列表以及 `:key`
和 `:test`
关键字参数，并返回一个新列表，其代表了在两个列表上进行适当的集合论操作所得到的结果：**INTERSECTION**
返回一个由两个参数中可找到的所有元素组成的列表。**UNION**
返回一个列表，其含有来自两个参数的每个唯一元素的一个实例。**SET-DIFFERENCE**
返回一个列表，其含有来自第一个参数但并不出现在第二个参数中的所有元素。而
**SET-EXCLUSIVE-OR**
则返回一个列表，其含有仅来自两个参数列表中的一个而不是两者的那些元素。这些函数中的每一个也都有一个相应的回收性函数，唯一区别在于后者的名字带有一个前缀 `N`。

Finally, the function **SUBSETP** takes two lists and the usual `:key` and
`:test` keyword arguments and returns true if the first list is a subset
of the second--if every element in the first list is also present in
the second list. The order of the elements in the lists doesn't
matter.

最后，函数 **SUBSETP** 接受两个列表以及通常的 `:key` 和 `:test`
关键字参数，并在第一个列表是第二个列表的一个子集时返回真，也就是说，第一个列表中的每一个元素也都存在于第二个列表中。列表中元素的顺序无关紧要。

```lisp
CL-USER> (subsetp '(3 2 1) '(1 2 3 4))
T
CL-USER> (subsetp '(1 2 3 4) '(3 2 1))
NIL
```
