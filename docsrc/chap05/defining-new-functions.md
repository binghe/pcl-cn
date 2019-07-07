# Defining New Functions（定义新函数）

Normally functions are defined using the **DEFUN** macro. The basic
skeleton of a **DEFUN** looks like this:

函数一般使用 **DEFUN** 宏来定义。**DEFUN** 的基本结构看起来像这样：

```lisp
(defun name (parameter*)
  "Optional documentation string."
  body-form*)
```

Any symbol can be used as a function name. Usually function names
contain only alphabetic characters and hyphens, but other characters
are allowed and are used in certain naming conventions. For instance,
functions that convert one kind of value to another sometimes use `->`
in the name. For example, a function to convert strings to widgets
might be called `string->widget`. The most important naming convention
is the one mentioned in Chapter 2, which is that you construct
compound names with hyphens rather than underscores or inner
caps. Thus, `frob-widget` is better Lisp style than either `frob_widget`
or `frobWidget`.

任何符号都可被用作函数名。 通常函数名仅包含字典字符和连字符，但是在特定的命名约定里，其他字符也被允许使用。例如，将值的一种类型转换成另一种的函数有时会在名字中使用
`->`。例如，一个将字符串转换成微件（widget）的函数可能被叫做
`string->widget`。最重要的一个命名约定是在第2章里提到的那个，即要用连字符而不是下划线或内部大写来构造复合名称。因此，`frob-widget`
比 `frob_widget` 或 `frobWidget` 具有 Lisp
风格。

A function's parameter list defines the variables that will be used to
hold the arguments passed to the function when it's called. If the
function takes no arguments, the list is empty, written as
`()`. Different flavors of parameters handle required, optional,
multiple, and keyword arguments. I'll discuss the details in the next
section.

一个函数的形参列表定义了一些变量，将被用来保存函数在调用时所传递的实参。如果函数不带有实参，则该列表就是空的，写成
`()`。不同种类的形参分别负责处理必要的、可选的、多重的以及关键字实参。我将在下一节里讨论相关细节。

If a string literal follows the parameter list, it's a documentation
string that should describe the purpose of the function. When the
function is defined, the documentation string will be associated with
the name of the function and can later be obtained using the
**DOCUMENTATION** function.

如果一个字符串紧跟在形参列表之后，那么它应该是一个用来描述函数用途的文档字符串。当定义函数时，该文档字符串将被关联到函数名上，并且以后可以通过
**DOCUMENTATION** 函数来获取。

Finally, the body of a **DEFUN** consists of any number of Lisp
expressions. They will be evaluated in order when the function is
called and the value of the last expression is returned as the value
of the function. Or the **RETURN-FROM** special operator can be used to
return immediately from anywhere in a function, as I'll discuss in a
moment.

最后，一个 **DEFUN** 的主体可由任意数量的 Lisp
表达式所构成。它们将在函数被调用时依次求值，而最后一个表达式的值将被作为整个函数的值返回。另外
**RETURN-FROM** 特殊操作符可被用于从函数的任何位置立即返回，我很快就会谈及它。

In Chapter 2 we wrote a `hello-world` function, which looked like this:

在第 2 章里我们写过一个 `hello-world` 函数，看起来像这样：

```lisp
(defun hello-world () (format t "hello, world"))
```

You can now analyze the parts of this function. Its name is
`hello-world`, its parameter list is empty so it takes no arguments, it
has no documentation string, and its body consists of one expression.

现在可以分析一下该程序的各个部分了。它的名字是 `hello-world`；形参列表为空，因此不接受任何参数；它没有文档字符串；并且它的函数体由一个表达式所构成：

```lisp
(format t "hello, world")
```

The following is a slightly more complex function:

下面是一个稍微更复杂一些的函数：

```lisp
(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
```

This function is named `verbose-sum`, takes two arguments that will be
bound to the parameters `x` and `y`, has a documentation string, and has
a body consisting of two expressions. The value returned by the call
to `+` becomes the return value of verbose-sum.
  
这个函数称为 `verbose-sum`，它接受的两个实参分别与形参
`x` 和 `y` 一一对应，并且带有一个文档字符串，以及一个由两个表达式所组成的主体。由
`+` 调用所返回的值将成为 `verbose-sum` 的返回值。
