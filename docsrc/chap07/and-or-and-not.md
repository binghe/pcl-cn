# AND, OR, and NOT（AND、OR 和 NOT）

When writing the conditions in **IF**, **WHEN**, **UNLESS**, and **COND** forms, three
operators that will come in handy are the boolean logic operators,
**AND**, **OR**, and **NOT**.

在使用 **IF**、**WHEN**、**UNLESS** 和 **COND**
形式编写条件语句时，经常用到的三个操作符是布尔逻辑操作符 **AND**、**OR**
和 **NOT**。

**NOT** is a function so strictly speaking doesn't belong in this chapter,
but it's closely tied to **AND** and **OR**. It takes a single argument and
inverts its truth value, returning **T** if the argument is **NIL** and **NIL**
otherwise.

严格来讲，**NOT** 这个函数并不属于本章讨论范畴，但它跟 **AND** 和 **OR**
紧密相关。它接受单一参数并对其真值取反，当参数为 **NIL** 时返回 **T**，否则返回 **NIL**。

AND and OR, however, are macros. They implement logical conjunction
and disjunction of any number of subforms and are defined as macros so
they can short-circuit. That is, they evaluate only as many of their
subforms--in left-to-right order--as necessary to determine the
overall truth value. Thus, AND stops and returns NIL as soon as one of
its subforms evaluates to NIL. If all the subforms evaluate to
non-NIL, it returns the value of the last subform. OR, on the other
hand, stops as soon as one of its subforms evaluates to non-NIL and
returns the resulting value. If none of the subforms evaluate to true,
OR returns NIL. Here are some examples:

而 **AND** 和 **OR**
则是宏。它们实现了对任意数量子表达式的逻辑合取和析取操作，并被定义成宏以便支持
“短路” 特性。也就是说，它们仅以从左到右的顺序对用于检测整体真值的必要数量的子表达式进行求值。这样，只要
**AND** 的一个子表达式求值为 **NIL**，它就立即停止并返回
**NIL**。如果所有子表达式都求值到非 **NIL**，那么它将返回最后一个子表达式的值。而对于
**OR** 来说只要一个子表达式求值到非
**NIL**，它就立即停止并返回当前子表达式的值。如果没有子表达式求值到真，**OR**
返回 **NIL**。下面是一些例子：

```lisp
(not nil)             ==> T
(not (= 1 1))         ==> NIL
(and (= 1 2) (= 3 3)) ==> NIL
(or (= 1 2) (= 3 3))  ==> T
```
