# DESTRUCTURING-BIND

One last tool for slicing and dicing lists that I need to cover since
you'll need it in later chapters is the **DESTRUCTURING-BIND** macro. This
macro provides a way to destructure arbitrary lists, similar to the
way macro parameter lists can take apart their argument list. The
basic skeleton of a **DESTRUCTURING-BIND** is as follows:

最后一个我需要介绍的，同时也是你将在后续章节里使用的一个用于拆分列表的工具是
**DESTRUCTURING-BIND**
宏。这个宏提供了一种解构（destructure）任意列表的方式，这类似于宏形参列表分拆它们的参数列表的方式。**DESTRUCTURING-BIND**
的基本骨架如下所示：

```lisp
(destructuring-bind (parameter*) list
  body-form*)
```

The parameter list can include any of the types of parameters
supported in macro parameter lists such as `&optional`, `&rest`, and `&key`
parameters. And, as in macro parameter lists, any parameter can be
replaced with a nested destructuring parameter list, which takes apart
the list that would otherwise have been bound to the replaced
parameter. The list form is evaluated once and should return a list,
which is then destructured and the appropriate values are bound to the
variables in the parameter list. Then the body-forms are evaluated in
order with those bindings in effect. Some simple examples follow:

该参数列表可以包含宏参数列表中支持的任何参数类型，比如 `&optional`、`&rest`
和 `&key`
参数。并且，如同在宏参数列表中一样，任何参数都可以被替换成一个嵌套的解构参数列表，从而将一个原本绑定在单个参数上的列表拆开。其中的
list 形式被求值一次并且应当返回一个列表，其随后被解构并且适当的值会被绑定到形参列表的对应变量中，然后那些
`body-form` 将在这些绑定的作用下被求值。一些简单的例子如下所示：

```lisp
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z)) ==> (:X 1 :Y 2 :Z 3)

(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z)) ==> (:X 1 :Y (2 20) :Z 3)

(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 20 :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 20 :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 NIL :Z 3)

(destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
  (list :x x :y y :z z)) ==> (:X 1 :Y 2 :Z 3)

(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z)) ==> (:X 3 :Y 2 :Z 1)
```

One kind of parameter you can use with **DESTRUCTURING-BIND** and also in
macro parameter lists, though I didn't mention it in Chapter 8, is a
`&whole` parameter. If specified, it must be the first parameter in a
parameter list, and it's bound to the whole list form. After a `&whole`
parameter, other parameters can appear as usual and will extract
specific parts of the list just as they would if the `&whole` parameter
weren't there. An example of using `&whole` with **DESTRUCTURING-BIND**
looks like this:

另外还有一种参数（尽管第 8 章并未介绍），它即可以用在
**DESTRUCTRING-BIND**
中可以用在宏参数列表中，这就是
`&whole`。如果被指定，它必须是参数列表中的第一个参数，并且它会被绑定到整个列表形式上。 在一个
`&whole` 参数之后，其他参数可以像通常那样出现并且将像没有
`&whole` 参数存在那样抽取出列表中的指定部分。一个将 `&whole` 与
**DESTRUCTURING-BIND** 同时使用的例子如下所示：

```lisp
(destructuring-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole))
==> (:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3))
```

You'll use a `&whole` parameter in one of the macros that's part of the
HTML generation library you'll develop in Chapter 31. However, I have
a few more topics to cover before you can get to that. After two
chapters on the rather Lispy topic of cons cells, you can now turn to
the more prosaic matter of how to deal with files and filenames.

你将在一个宏里使用 `&whole`
参数，它是你将在第 31 章里开发的 HTML
生成库的一部分。不过在那之前，我还要谈及更多的主题。在关于点对单元的两章相当
Lisp 化的主题之后，下面将介绍如何处理文件和文件名这个相对乏味的问题。
