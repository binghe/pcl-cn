# "Destructive" Operations（“破坏性”操作）

If Common Lisp were a purely functional language, that would be the
end of the story. However, because it's possible to modify a cons cell
after it has been created by SETFing its CAR or CDR, you need to think
a bit about how side effects and structure sharing mix.

如果 Common Lisp
只是一门纯函数式语言，那么故事就应该到此为止了。不过，因为在一个点对单元被创建之后有可能通过对其
**CAR** 或 **CDR** 进行 **SETF**
操作来修改它，你需要想一想副作用是如何跟结构共享混合的。

Because of Lisp's functional heritage, operations that modify existing
objects are called destructive--in functional programming, changing an
object's state "destroys" it since it no longer represents the same
value. However, using the same term to describe all state-modifying
operations leads to a certain amount of confusion since there are two
very different kinds of destructive operations, *for-side-effect*
operations and *recycling* operations.

由于 Lisp
的函数式传统，修改已有对象的操作被称作是破坏性的（destructive）。在函数式编程中，改变一个对象的状态相当于
“破坏”
了它，因为它不再代表相同的那个值了。尽管如此，使用同样的术语来描述所有的状态修改操作将导致一定程度上的误解，因为存在两种相当不同破坏性操作类型，即副作用性（for-side-effect）操作和回收性（recycling）操作。

For-side-effect operations are those used specifically for their side
effects. All uses of **SETF** are destructive in this sense, as are
functions that use **SETF** under the covers to change the state of an
existing object such as **VECTOR-PUSH** or **VECTOR-POP**. But it's a bit
unfair to describe these operations as destructive--they're not
intended to be used in code written in a functional style, so they
shouldn't be described using functional terminology. However, if you
mix nonfunctional, for-side-effect operations with functions that
return structure-sharing results, then you need to be careful not to
inadvertently modify the shared structure. For instance, consider
these three definitions:

副作用性操作是那些专门利用其副作用的操作。就此而言，所有对 **SETF**
的使用都是破坏性的，此外还包括诸如 **VECTOR-PUSH** 或 **VECTOR-POP**
这类在底层使用 **SETF**
来修改已有对象状态的函数。但是将这些操作描述成是破坏性的有一点不公平——它们没打算被用于以函数式风格编写的代码中，因此你不该用函数式术语来描述它们。如果将非函数式的副作用性操作和那些返回结构共享结果的函数混合使用，那么需要小心不要疏忽地修改了共享的结构。例如，考虑下面三个定义：

```lisp
(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
```

After evaluating these forms, you have three lists, but `*list-3*` and
`*list-2*` share structure just like the lists in the previous diagram.

在对这些形式求值之后，你有了三个列表，但是 `*list-3*` 和 `*list-2*`
就像前面的图示中的列表那样共享了一些结构。

```lisp
*list-1*                  ==> (1 2)
*list-2*                  ==> (3 4)
*list-3*                  ==> (1 2 3 4)
```

Now consider what happens when you modify `*list-2*`.

现在看看当你修改了 `*list-2*` 时会发生什么：

```lisp
(setf (first *list-2*) 0) ==> 0
*list-2*                  ==> (0 4)     ; as expected
*list-3*                  ==> (1 2 0 4) ; maybe not what you wanted
```

The change to `*list-2*` also changes `*list-3*` because of the shared
structure: the first cons cell in `*list-2*` is also the third cons cell
in `*list-3*`. SETFing the FIRST of `*list-2*` changes the value in the
**CAR** of that cons cell, affecting both lists.

在共享的结构中，由于 `*list-2*` 中的第一个点对单元也是 `*list-3*`
中的第三个点对单元，对 `*list-2*` 的改变也改变了 `*list-3*`，对
`*list-2*` 的 **FIRST** 进行 **SETF**
改变了该点对单元中 **CAR** 部分的值，从而影响了两个列表。

On the other hand, the other kind of destructive operations, recycling
operations, are intended to be used in functional code. They use side
effects only as an optimization. In particular, they reuse certain
cons cells from their arguments when building their result. However,
unlike functions such as **APPEND** that reuse cons cells by including
them, unmodified, in the list they return, recycling functions reuse
cons cells as raw material, modifying the **CAR** and **CDR** as necessary to
build the desired result. Thus, recycling functions can be used safely
only when the original lists aren't going to be needed after the call
to the recycling function.

而另一种破坏性操作，即回收性操作，其本来就用于函数式代码中的。它们的副作用仅是一种优化手段。特别地，它们在构造结果时会重用来自它们实参的特定点对单元。尽管如此，和诸如
**APPEND**
这种在返回列表中包含未经修改的点对单元的函数有所不同的是，回收性函数将点对单元作为原材料来重用，如有必要它将修改其
**CAR** 和 **CDR**
来构造想要的结果。这样，只有当调用回收性函数之后不再需要原先列表的情况下，回收性函数才可以被安全地使用。

To see how a recycling function works, let's compare **REVERSE**, the
nondestructive function that returns a reversed version of a sequence,
to **NREVERSE**, a recycling version of the same function. Because **REVERSE**
doesn't modify its argument, it must allocate a new cons cell for each
element in the list being reversed. But suppose you write something
like this:

为了观察回收性函数是怎样工作的，让我们将
**REVERSE**，即返回一个序列的逆序版本的非破坏性函数，与它的回收性版本
**NREVERSE** 进行比较。由于 **REVERSE**
不修改其参数，它必须为将要逆序的列表的每个元素分配一个新的点对单元。但假如写出了类似下面的代码：

```lisp
(setf *list* (reverse *list*))
```

By assigning the result of **REVERSE** back to `*list*`, you've removed the
reference to the original value of `*list*`. Assuming the cons cells in
the original list aren't referenced anywhere else, they're now
eligible to be garbage collected. However, in many Lisp
implementations it'd be more efficient to immediately reuse the
existing cons cells rather than allocating new ones and letting the
old ones become garbage.

通过将 **REVERSE** 的结果赋值回 `*list*`，你就删除了对 `*list*`
原先的值的引用。假设原先列表中的点对单元不被任何其他位置引用，它们现在可以被作为垃圾收集了。不过，在许多
Lisp 实现中，立即重用已有的点对单元会比分配新的点对单元并让老的变成垃圾更加高效。

**NREVERSE** allows you to do exactly that. The `N` stands for non-consing,
meaning it doesn't need to allocate any new cons cells. The exact side
effects of **NREVERSE** are intentionally not specified--it's allowed to
modify any **CAR** or **CDR** of any cons cell in the list--but a typical
implementation might walk down the list changing the **CDR** of each cons
cell to point to the previous cons cell, eventually returning the cons
cell that was previously the last cons cell in the old list and is now
the head of the reversed list. No new cons cells need to be allocated,
and no garbage is created.

**NREVERSE** 就能让你这么做。函数名字中 `N` 的含义是
*non-consing*，意思是它不需要分配任何新的点对单元。虽然故意没有说明
**NREVERSE** 的明确的副作用（它可以修改列表中任何点对单元的任何
**CAR** 或 **CDR**），但典型的实现可能会沿着列表依次改变每个点对单元的
**CDR**，使其指向前一个点对单元。最终返回的点对单元曾经是旧列表的最后一个点对单元，而现在则成为逆序后的列表的头节点。不需要分配新的点对单元，也没有产生垃圾。

Most recycling functions, like **NREVERSE**, have nondestructive
counterparts that compute the same result. In general, the recycling
functions have names that are the same as their non-destructive
counterparts except with a leading `N`. However, not all do, including
several of the more commonly used recycling functions such as **NCONC**,
the recycling version of **APPEND**, and **DELETE**, **DELETE-IF**, **DELETE-IF-NOT**,
and **DELETE-DUPLICATES**, the recycling versions of the **REMOVE** family of
sequence functions.

像 **NREVERSE**
这样的回收性函数大多都带有对应的能产生相同结果但却没有破坏性的同伴函数。一般来说，回收性函数和它们的非破坏性同伴带有相同的名字，除了一个起始的字母
`N`。尽管如此，并非所有函数都是这样，其中包括几个更常用的回收性函数。例如
**NCONC**，它是 **APPEND**
的回收性版本；而 **DELETE**、**DELETE-IF**、**DELETE-IF-NOT** 和
**DELETE-DUPLICATES** 则是序列函数的 **REMOVE** 家族的回收性版本。

In general, you use recycling functions in the same way you use their
nondestructive counterparts except it's safe to use them only when you
know the arguments aren't going to be used after the function
returns. The side effects of most recycling functions aren't specified
tightly enough to be relied upon.

总之，你可以用与回收性函数的非破坏性同伴相同的方式来使用它们，但只有当你知道参数在函数返回之后不再被使用时，才能安全地使用它们。多数回收性函数的副作用说明并未严格到足以信赖的程度。

However, the waters are further muddied by a handful of recycling
functions with specified side effects that can be relied upon. They
are **NCONC**, the recycling version of **APPEND**, and **NSUBSTITUTE** and its
`-IF` and `-IF-NOT` variants, the recycling versions of the sequence
functions **SUBSTITUTE** and friends.

但有一组回收性函数带有可靠的明确指定的副作用，这使得事情变得更复杂了。它们是
**NCONC**，即 **APPEND** 的回收性版本，以及 **NSUBSTITUTE** 和它的
`-IF` 和 `-IF-NOT` 变体，这些是序列函数 **SUBSTITUTE** 及其变体的回收性版本。

Like **APPEND**, **NCONC** returns a concatenation of its list arguments, but
it builds its result in the following way: for each nonempty list it's
passed, **NCONC** sets the **CDR** of the list's last cons cell to point to
the first cons cell of the next nonempty list. It then returns the
first list, which is now the head of the spliced-together
result. Thus:

和 **APPEND** 一样，**NCONC**
返回其列表实参的连结体，但是它以下面的方式构造其结果：对于传递给它的每
一个非空列表，**NCONC** 会将该列表的最后一个点对单元的 **CDR**
设置成指向下一个非空列表的第一个点对单元。然后它返回第一个列表，后者现在是拼接在一起的结果的开始部分。结果如下所示：

```lisp
(defparameter *x* (list 1 2 3))

(nconc *x* (list 4 5 6)) ==> (1 2 3 4 5 6)

*x* ==> (1 2 3 4 5 6)
```

**NSUBSTITUTE** and variants can be relied on to walk down the list
structure of the list argument and to **SETF** the **CAR**s of any cons cells
holding the old value to the new value and to otherwise leave the list
intact. It then returns the original list, which now has the same
value as would've been computed by **SUBSTITUTE**.

**NSUBSTITUTE** 及其变体可靠地沿着列表实参的列表结构向下遍历，将任何带有旧值的点对单元的
**CAR** 部分 **SETF**
到新的值上，否则保持列表原封不动。然后它返回最初的列表，其带有与
**SUBSTITUTE** 计算得到的结果相同的值。

The key thing to remember about **NCONC** and **NSUBSTITUTE** is that they're
the exceptions to the rule that you can't rely on the side effects of
recycling functions. It's perfectly acceptable--and arguably good
style--to ignore the reliability of their side effects and use them,
like any other recycling function, only for the value they return.

关于 **NCONC** 和
**NSUBSTITUTE**，关键是需要记住，它们是不能依赖于回收性函数的副作用这一规则的例外。忽视它们副作用可靠性，而像任何其他回收性函数一样来使用它们，只用来产生返回值，这种做法不但完全可以接受，甚至还是一种好的编程风格。

