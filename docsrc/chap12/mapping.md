# Mapping（映射）

Another important aspect of the functional style is the use of
higher-order functions, functions that take other functions as
arguments or return functions as values. You saw several examples of
higher-order functions, such as **MAP**, in the previous chapter. Although
**MAP** can be used with both lists and vectors (that is, with any kind of
sequence), Common Lisp also provides six mapping functions
specifically for lists. The differences between the six functions have
to do with how they build up their result and whether they apply the
function to the elements of the list or to the cons cells of the list
structure.

函数式风格的另一个重要方面是对高阶函数的使用，即那些接受其他函数作为参数或将函数作为返回值的函数。前面章节里出现过几个高阶函数，例如
**MAP**。尽管MAP可被同时用于列表和向量（也就是说，任何类型的序列），但
Common Lisp 另外还提供了 6 个特定用于列表的映射函数。这 6
个函数之间的区别在于它们构造结果的方式，以及它们究竟会将函数应用到列表的元素还是列表结构的点对单元上。

**MAPCAR** is the function most like **MAP**. Because it always returns a
list, it doesn't require the result-type argument **MAP** does. Instead,
its first argument is the function to apply, and subsequent arguments
are the lists whose elements will provide the arguments to the
function. Otherwise, it behaves like **MAP**: the function is applied to
successive elements of the list arguments, taking one element from
each list per application of the function. The results of each
function call are collected into a new list. For example:

**MAPCAR** 是最接近 **MAP**
的函数。因为它总是返回一个列表，所以它并不要求 **MAP**
所要求的结果类型实参。替代，它的第一个参数是想要应用的函数，而后续参数是其元素将为该函数提供实参的列表。除此之外，它和
**MAP** 的行为相同：函数被应用在列表实参的相继元素上，每次函数的应用会从每个列表中各接受一个元素。每次函数调用的结果都被收集到一个新列表中。例如：

```lisp
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3)) ==> (2 4 6)
(mapcar #'+ (list 1 2 3) (list 10 20 30)) ==> (11 22 33)
```

**MAPLIST** is just like **MAPCAR** except instead of passing the elements of
the list to the function, it passes the actual cons cells. Thus, the
function has access not only to the value of each element of the list
(via the **CAR** of the cons cell) but also to the rest of the list (via
the **CDR**).

他们之间的区别在于 **MAPLIST**
传递给函数的不是列表元素而是实际的点对单元。这样，该函数不仅可以访问到列表中每个元素的值（通过点对单元的
**CAR**），还可以访问到列表的其余部分（通过 **CDR**）。

**MAPCAN** and **MAPCON** work like **MAPCAR** and **MAPLIST** except for the way they
build up their result. While **MAPCAR** and **MAPLIST** build a completely new
list to hold the results of the function calls, **MAPCAN** and **MAPCON**
build their result by splicing together the results--which must be
lists--as if by **NCONC**. Thus, each function invocation can provide any
number of elements to be included in the result. **MAPCAN**, like
**MAPCAR**, passes the elements of the list to the mapped function while
**MAPCON**, like **MAPLIST**, passes the cons cells.

除了构造结果的方式不同之外，**MAPCAN** 和 **MAPCON** 与 **MAPCAR** 和
**MAPLIST** 的工作方式也很相似。**MAPCAR** 和 **MAPLIST**
会构造一个全新的列表来保存函数调用的结果，而 **MAPCAN** 和 **MAPCON**
则通过将结果（必须是列表）用 **NCONC**
拼接在一起来产生它们的结果。这样，每次函数调用都可以向结果中提供任意数量的元素。**MAPCAN**
像 **MAPCAR** 那样把列表的元素传递到映射函数中，而 **MAPCON** 则像 **MAPLIST**
那样来传递点对单元。

Finally, the functions **MAPC** and **MAPL** are control constructs disguised
as functions--they simply return their first list argument, so they're
useful only when the side effects of the mapped function do something
interesting. **MAPC** is the cousin of **MAPCAR** and **MAPCAN** while **MAPL** is in
the **MAPLIST**/**MAPCON** family.

最后，函数 **MAPC** 和 **MAPL**
是伪装成函数的控制构造，它们只返回第一个列表实参，因此只有当映射函数的副作用有用时，它们才是有用的。**MAPC**
是 **MAPCAR** 和 **MAPCAN** 的近亲，而 **MAPL** 属于
**MAPLIST**/**MAPCON** 家族。
