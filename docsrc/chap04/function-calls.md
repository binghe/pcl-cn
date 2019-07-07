# Function Calls（函数调用）

The evaluation rule for function call forms is simple: evaluate the
remaining elements of the list as Lisp forms and pass the resulting
values to the named function. This rule obviously places some
additional syntactic constraints on a function call form: all the
elements of the list after the first must themselves be well-formed
Lisp forms. In other words, the basic syntax of a function call form
is as follows, where each of the arguments is itself a Lisp form:

函数调用形式的求值规则很简单：对列表的其余元素作为 Lisp
形式进行求值并将结果传递到命名函数中。这一规则明显的有着一些附加的句法限制在函数调用形式上：除第一个以外，所有的列表元素它们自身必须是一个形态良好的
Lisp 形式。换句话说，函数调用形式的基本语法应如下所示，其中每个参数其本身也是一个
Lisp 形式：

```lisp
(function-name argument*)
```

Thus, the following expression is evaluated by first evaluating 1,
then evaluating 2, and then passing the resulting values to the `+`
function, which returns 3:

这样下面这个表达式在求值时将首先求值 1，再求值 2，然后将得到的值传给
`+` 函数，再返回 3:

```lisp
(+ 1 2)
```

A more complex expression such as the following is evaluated in
similar fashion except that evaluating the arguments `(+ 1 2)` and
`(- 3 4)` entails first evaluating their arguments and applying the
appropriate functions to them:

像下面这样更复杂的表达式也采用相似的求值方法,不过在求值参数 `(+ 1 2)`
和 `(- 3 4)` 时需要先对它们的参数求值，然后再对它们应用相应的函数：

```lisp
(* (+ 1 2) (- 3 4))
```

Eventually, the values 3 and -1 are passed to the `*` function, which
returns -3.

最后，值 3 和 -1 被传递到 `*` 函数里，从而得到 -3。

As these examples show, functions are used for many of the things that
require special syntax in other languages. This helps keep Lisp's
syntax regular.

正如这些例子所显示的这样，许多其他语言中需要特殊语法来处理的事务在 Lisp
中都可用函数来处理。Lisp 的这种设计对于保持其语法正则化很有帮助。
