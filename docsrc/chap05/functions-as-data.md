# Functions As Data, a.k.a. Higher-Order Functions（作为数据的函数——高阶函数）

While the main way you use functions is to call them by name, a number
of situations exist where it's useful to be able treat functions as
data. For instance, if you can pass one function as an argument to
another, you can write a general-purpose sorting function while
allowing the caller to provide a function that's responsible for
comparing any two elements. Then the same underlying algorithm can be
used with many different comparison functions. Similarly, callbacks
and hooks depend on being able to store references to code in order to
run it later. Since functions are already the standard way to abstract
bits of code, it makes sense to allow functions to be treated as data.

使用函数的主要方式是通过名字来调用它们，但有时将函数作为数据看待也是很有用的。例如，可以将一个函数作为参数传给另一个函数，从而能写出一个通用的排序函数，允许调用者提供一个比较任意两元素的函数，这样同样的底层算法就可以跟许多不同的比较函数配合使用了。类似地，回调函数（callback）和钩子（hook）也需要能够保存代码引用便于以后运行。由于函数已经是一种对代码比特进行抽象的标准方式，因此把允许函数视为数据也是合理的。

In Lisp, functions are just another kind of object. When you define a
function with **DEFUN**, you're really doing two things: creating a new
function object and giving it a name. It's also possible, as you saw
in Chapter 3, to use **LAMBDA** expressions to create a function without
giving it a name. The actual representation of a function object,
whether named or anonymous, is opaque--in a native-compiling Lisp, it
probably consists mostly of machine code. The only things you need to
know are how to get hold of it and how to invoke it once you've got
it.

在 Lisp 中，函数只是另一种类型的对象。被用 **DEFUN**
定义一个函数时，实际上做了两件事：创建一个新的函数对象以及赋予其一个名字。在第 3
章里我们看到，也可以使用 **LAMBDA**
表达式来创建一个函数而无需为其指定一个名字。一个函数对象的实际表示，无论是有名的还是匿名的，都只是一些二进制数据——以原生编译的
Lisp 形式存在，可能大部分是由机器码构成。只需要知道如何保持它们以及需要时如何调用它们。

The special operator **FUNCTION** provides the mechanism for getting at a
function object. It takes a single argument and returns the function
with that name. The name isn't quoted. Thus, if you've defined a
function foo, like so:

特别操作符 **FUNCTION**
提供了用来获取一个函数对象的方法。它接受单一实参并返回该参数同名的函数。这个名字是不被引用的。因此如果一个函数
`foo` 的定义如下：

```lisp
CL-USER> (defun foo (x) (* 2 x))
FOO
```

you can get the function object like this:

就可以得到如下的函数对象：

```lisp
CL-USER> (function foo)
#<Interpreted Function FOO>
```

In fact, you've already used **FUNCTION**, but it was in disguise. The
syntax `#'`, which you used in Chapter 3, is syntactic sugar for
**FUNCTION**, just the way ' is syntactic sugar for **QUOTE**. Thus, you can
also get the function object for foo like this:

事实上，你已经用过 **FUNCTION** 了，但它是以伪装的形式出现的。第 3
章里用到的 `#'` 语法就是 **FUNCTION** 的语法糖，正如 `'`
是 **QUOTE** 的语法糖一样。 因此也可以像这样得到 `foo` 的函数对象。

```lisp
CL-USER> #'foo
#<Interpreted Function FOO>
```

Once you've got the function object, there's really only one thing you
can do with it--invoke it. Common Lisp provides two functions for
invoking a function through a function object: **FUNCALL** and **APPLY**.
They differ only in how they obtain the arguments to pass to the
function.

一旦得到了函数对象，就只剩下一件事可做了——调用它。Common Lisp
提供了两个函数用来通过函数对象调用函数：**FUNCALL** 和
**APPLY**。 它们的区别仅在于如何获取传递给函数的实参。

**FUNCALL** is the one to use when you know the number of arguments you're
going to pass to the function at the time you write the code. The
first argument to **FUNCALL** is the function object to be invoked, and
the rest of the arguments are passed onto that function. Thus, the
following two expressions are equivalent:

**FUNCALL** 用于在编写代码时确切知道传递给函数多少实参时。**FUNCALL**
的第一个实参是被调用的函数对象，其余的实参被传递到该函数中。因此，下面两个表达式是等价的：

```lisp
(foo 1 2 3) === (funcall #'foo 1 2 3)
```

However, there's little point in using **FUNCALL** to call a function
whose name you know when you write the code. In fact, the previous two
expressions will quite likely compile to exactly the same machine
instructions.

不过，用 **FUNCALL**
来调用一个写代码时名字已知的函数毫无意义。事实上，前面的两个表达式将很可能被编译成相同的机器指令。

The following function demonstrates a more apt use of **FUNCALL**. It
accepts a function object as an argument and plots a simple ASCII-art
histogram of the values returned by the argument function when it's
invoked on the values from `min` to `max`, stepping by `step`.

下面这个函数演示了 **FUNCALL**
的另一个更有建设性的用法。它接受一个函数对象作为实参，并使用实参函数在
`min` 和 `max` 之间以 `step` 为步长的返回值来绘制一个简单的 ASCII 艺术条形图：

```lisp
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
```

The **FUNCALL** expression computes the value of the function for each
value of i. The inner **LOOP** uses that computed value to determine how
many times to print an asterisk to standard output.

**FUNCALL** 表达式在每个 `i` 值上计算函数的值。内层 **LOOP**
循环使用计算得到的值来决定向标准输出打印多少星号。

Note that you don't use **FUNCTION** or `#'` to get the function value of
fn; you want it to be interpreted as a variable because it's the
variable's value that will be the function object. You can call plot
with any function that takes a single numeric argument, such as the
built-in function **EXP** that returns the value of `e` raised to the power
of its argument.

请注意，不需要使用 **FUNCTION** 或 `#'` 来得到 `fn`
的函数值。因为它是作为函数对象的变量的值，所以你需要它被解释成一个变量。可以用任何接受单一数值实参的函数来调用
`plot`，例如内置的函数 **EXP**，它返回以 `e` 为底而以实参为指数的值。

```lisp
CL-USER> (plot #'exp 0 4 1/2)
*
*
**
****
*******
************
********************
*********************************
******************************************************
NIL
```

**FUNCALL**, however, doesn't do you any good when the argument list is
known only at runtime. For instance, to stick with the `plot` function
for another moment, suppose you've obtained a list containing a
function object, a minimum and maximum value, and a step value. In
other words, the list contains the values you want to pass as
arguments to `plot`. Suppose this list is in the variable `plot-data`. You
could invoke `plot` on the values in that list like this:

然而，当实参列表只在运行期已知时，**FUNCALL**
的表现不佳。例如，为了再次调用 `plot`
函数，假设你已有一个列表，其包括一个函数对象，一个最小值和一个最大值以及一个步长。换句话说，这个列表包含了你想要作为实参传给
`plot` 的所有的值。假设这个列表保存在变量 `plot-data`
中，可以像这样用列表中的值来调用 `plot`：

```lisp
(plot (first plot-data) (second plot-data) (third plot-data) (fourth plot-data))
```

This works fine, but it's pretty annoying to have to explicitly unpack
the arguments just so you can pass them to plot.

这样固然可以，但仅仅为了将实参传给 `plot` 而显式地将其解开，看起来相当讨厌。

That's where **APPLY** comes in. Like **FUNCALL**, the first argument to **APPLY**
is a function object. But after the function object, instead of
individual arguments, it expects a list. It then applies the function
to the values in the list. This allows you to write the following
instead:

这就是需要 **APPLY** 的原因。和 **FUNCALL** 一样，**APPLY**
的第一个参数是一个函数对象。但在这个函数对象之后，它期待一个列表而非单独的实参。它将函数应用在列表中的值上，这就使你可以写出下面的替代版本：

```lisp
(apply #'plot plot-data)
```

As a further convenience, **APPLY** can also accept "loose" arguments as
long as the last argument is a list. Thus, if plot-data contained just
the min, max, and step values, you could still use **APPLY** like this to
plot the **EXP** function over that range:

更方便的是，**APPLY** 还接受 “孤立”（loose）的实参只要最后一个参数是个列表。因此，假如
`plot-data` 只含有最小、最大和步长值，那么你仍然可以像这样来使用
**APPLY** 在该范围上绘制 **EXP** 函数：

```lisp
(apply #'plot #'exp plot-data)
```

**APPLY** doesn't care about whether the function being applied takes
`&optional`, `&rest`, or `&key` arguments--the argument list produced by
combining any loose arguments with the final list must be a legal
argument list for the function with enough arguments for all the
required parameters and only appropriate keyword parameters.

**APPLY** 并不关心所用的函数是否接受 `&optional`、`&rest` 或是 `&key`
实参——由任何孤立实参和最后的列表所组合而成的实参列表必定是一个合法的实参列表，其对于该函数来说带有足够的实参用于所有必要形参和适当的关键字形参。
