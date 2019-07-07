# Optional Parameters（可选形参）

While many functions, like `verbose-sum`, need only required
parameters, not all functions are quite so simple. Sometimes a
function will have a parameter that only certain callers will care
about, perhaps because there's a reasonable default value. An example
is a function that creates a data structure that can grow as
needed. Since the data structure can grow, it doesn't matter--from a
correctness point of view--what the initial size is. But callers who
have a good idea how many items they're going to put into the data
structure may be able to improve performance by specifying a specific
initial size. Most callers, though, would probably rather let the code
that implements the data structure pick a good general-purpose
value. In Common Lisp you can accommodate both kinds of callers by
using an optional parameter; callers who don't care will get a
reasonable default, and other callers can provide a specific value.

虽然许多像 `verbose-sum`
这样的函数只有必要形参，但并非所有函数都如此简单。有时一个函数将带有一个只有特定调用者才会关心的形参，这可能是因为它有一个合理的默认值。例如一个可以创建按需增长的数据结构的函数。由于数据结构可以增长，那么从正确性角度来说，它的初始尺寸就无关紧要了。那些清楚知道自己打算在数据结构中放置多少个元素的调用者们，可以通过设置特定的初始尺寸来改进其程序的性能，而多数调用者只需让实现数据结构的代码自行选择一个好的通用值就可以了。在
Common Lisp 中，你可以使用可选形参，从而使两类调用者都满意。不在意的调用者们将得到一个合理的默认值，而其他调用者们有机会提供一个指定的值。

To define a function with optional parameters, after the names of any
required parameters, place the symbol &optional followed by the names
of the optional parameters. A simple example looks like this:

为了定义一个带有可选形参的函数，在必要形参的名字之后放置符号
`&optional`，后接可选形参的名字。下面就是一个简单的例子：

```lisp
(defun foo (a b &optional c d) (list a b c d))
```

When the function is called, arguments are first bound to the required
parameters. After all the required parameters have been given values,
if there are any arguments left, their values are assigned to the
optional parameters. If the arguments run out before the optional
parameters do, the remaining optional parameters are bound to the
value **NIL**. Thus, the function defined previously gives the following
results:

当该函数被调用时，实参被首先绑定到必要形参上。在所有必要形参都被赋值以后，如果还有任何实参剩余，它们的值将被赋给可选形参。如果实参在所有可选形参被赋值之前用完了，那么其余的可选形参将自动绑定到值
**NIL** 上。这样，前面定义的函数会给出下面的结果：

```lisp
(foo 1 2)     ==> (1 2 NIL NIL)
(foo 1 2 3)   ==> (1 2 3 NIL)
(foo 1 2 3 4) ==> (1 2 3 4)
```

Lisp will still check that an appropriate number of arguments are
passed to the function--in this case between two and four,
inclusive--and will signal an error if the function is called with too
few or too many.

Lisp 仍然可以确保适当数量的实参被传递给函数——在本例中是 2 到 4
个。而如果函数用太少或太多的参数来调用的话，将会报错。

Of course, you'll often want a different default value than
**NIL**. You can specify the default value by replacing the parameter
name with a list containing a name and an expression. The expression
will be evaluated only if the caller doesn't pass enough arguments to
provide a value for the optional parameter. The common case is simply
to provide a value as the expression.

当然，你会经常想要一个不同于
**NIL** 的默认值。这时可以通过将形参名替换成一个含有名字跟一个表达式的列表来指定该默认值。只有在调用者没有传递足够的实参来为可选形参提供值的时候，这个表达式才会被求值。通常情况只是简单地提供一个值作为表达式：

```lisp
(defun foo (a &optional (b 10)) (list a b))
```

This function requires one argument that will be bound to the
parameter `a`. The second parameter, `b`, will take either the value of
the second argument, if there is one, or 10.

上述函数要求将一个实参绑定到形参 `a` 上。当存在第二个实参时，第二个形参
`b` 将使用其值，否则使用 10。

```lisp
(foo 1 2) ==> (1 2)
(foo 1)   ==> (1 10)
```

Sometimes, however, you may need more flexibility in choosing the
default value. You may want to compute a default value based on other
parameters. And you can--the default-value expression can refer to
parameters that occur earlier in the parameter list. If you were
writing a function that returned some sort of representation of a
rectangle and you wanted to make it especially convenient to make
squares, you might use an argument list like this:

不过有时可能需要更灵活地选择默认值。比如可能想要基于其他形参来计算默认值。默认值表达式可以引用早先出现在形参列表中的形参。如果要编写一个返回矩形的某种表示的函数，并且想要使它可以特别方便地产生正方形，那么可以使用一个像这样的形参列表：

```lisp
(defun make-rectangle (width &optional (height width)) ...)
```

which would cause the height parameter to take the same value as the
width parameter unless explicitly specified.

除非明确指定否则这将导致 `height` 形参带有和 `width` 形参相同的值。

Occasionally, it's useful to know whether the value of an optional
argument was supplied by the caller or is the default value. Rather
than writing code to check whether the value of the parameter is the
default (which doesn't work anyway, if the caller happens to
explicitly pass the default value), you can add another variable name
to the parameter specifier after the default-value expression. This
variable will be bound to true if the caller actually supplied an
argument for this parameter and **NIL** otherwise. By convention, these
variables are usually named the same as the actual parameter with a
`-supplied-p` on the end. For example:

有时，有必要去了解一个可选形参的值究竟是被调用者明确指定还是使用了默认值。除了通过代码来检查形参的值是否为默认值（假如调用者碰巧显式传递了默认值，那么这样做终归是无效的）以外，你还可以通过在形参标识符的默认值表达式之后添加另一个变量名来做到这点。该变量将在调用者实际为该形参提供了一个实参时被绑定到真值，否则为
**NIL**。通常约定，这种变量的名字与对应的真实形参相同，但是带有一个
`-supplied-p` 后缀。例如：

```lisp
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
```

This gives results like this:

这将给出类似下面的结果：

```lisp
(foo 1 2)   ==> (1 2 3 NIL)
(foo 1 2 3) ==> (1 2 3 T)
(foo 1 2 4) ==> (1 2 4 T)
```
