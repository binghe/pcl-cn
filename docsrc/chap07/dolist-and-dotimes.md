# **DOLIST** and **DOTIMES**（**DOLIST** 和 **DOTIMES**）

I'll start with the easy-to-use **DOLIST** and **DOTIMES** macros.

先从易于使用的 **DOLIST** 和 **DOTIMES** 宏开始。

**DOLIST** loops across the items of a list, executing the loop body with
a variable holding the successive items of the list. This is the
basic skeleton (leaving out some of the more esoteric options):

**DOLIST**
在一个列表的元素上循环操作，使用一个依次持有列表中所有后继元素的变量来执行循环体。下面是其基本形式（去掉了一些比较难懂的选项）：

```lisp
(dolist (var list-form)
  body-form*)
```

When the loop starts, the `list-form` is evaluated once to produce a
list. Then the body of the loop is evaluated once for each item in
the list with the variable `var` holding the value of the item. For
instance:

当循环开始时，`list-form`
被求值一次以产生一个列表。然后循环体在列表的每一项上求值一次，同时用变量
`var` 保存当前项的值。例如：

```lisp
CL-USER> (dolist (x '(1 2 3)) (print x))
1
2
3
NIL
```

Used this way, the **DOLIST** form as a whole evaluates to **NIL**.

在这种方式下，**DOLIST** 这种形式本身求值为 **NIL**。

If you want to break out of a **DOLIST** loop before the end of the list,
you can use **RETURN**.

如果想在列表结束之前中断一个 **DOLIST** 循环，则可以使用 **RETURN**。

```lisp
CL-USER> (dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))
1
2
NIL
```

**DOTIMES** is the high-level looping construct for counting loops. The
basic template is much the same as **DOLIST**'s.

**DOTIMES** 是用于循环计数的高级循环构造。其基本模板和 **DOLIST** 非常相似。

```lisp
(dotimes (var count-form)
  body-form*)
```

The `count-form` must evaluate to an integer. Each time through the loop
`var` holds successive integers from 0 to one less than that number. For
instance:

其中的 `count-form` 必须要能求值为一个整数。通过每次循环，`var`
所持有的整数依次为从 0 到比那个数小 1 的每一个后继整数。例如：

```lisp
CL-USER> (dotimes (i 4) (print i))
0
1
2
3
NIL
```

As with **DOLIST**, you can use **RETURN** to break out of the loop early.

和 **DOLIST** 一样，也可以使用 **RETURN** 来提前中断循环。

Because the body of both **DOLIST** and **DOTIMES** loops can contain any kind
of expressions, you can also nest loops. For example, to print out the
times tables from 1 × 1 = 1 to 20 × 20 = 400, you can write this pair
of nested **DOTIMES** loops:

由于 **DOLIST** 和 **DOTIMES**
的循环体中可以包含任何类型的表达式，因此也可以使用嵌套循环。例如，为了打印出从
1 × 1 = 1 到 20 × 20 = 400 的乘法表，可以写出下面这对嵌套的
**DOTIMES** 循环：

```lisp
(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))
```
