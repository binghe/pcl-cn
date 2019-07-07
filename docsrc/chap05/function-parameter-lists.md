# Function Parameter Lists（函数形参列表）

There's not a lot more to say about function names or documentation
strings, and it will take a good portion of the rest of this book to
describe all the things you can do in the body of a function, which
leaves us with the parameter list.

关于函数名或文档字符串就没有更多可说的了，而本书其余部分将用很多篇幅来描述所有可在一个函数体里做的事情，因此就只需讨论形参列表了。

The basic purpose of a parameter list is, of course, to declare the
variables that will receive the arguments passed to the function. When
a parameter list is a simple list of variable names--as in
`verbose-sum`--the parameters are called _required parameters_. When a
function is called, it must be supplied with one argument for every
required parameter. Each parameter is bound to the corresponding
argument. If a function is called with too few or too many arguments,
Lisp will signal an error.

很明显，一个形参列表的基本用途是为了声明一些变量，用来接收传递给函数的实参。当形参列表是一个由变量名所组成的简单列表时，如同在
`verbose-sum` 里那样，这些形参被称为必要形参。当函数被调用时，必须为它的每一个必要形参都提供一个实参。每一个形参被绑定到对应的实参上。如果一
个函数以过少或过多的实参来调用的话，Lisp 就会报错。

However, Common Lisp's parameter lists also give you more flexible
ways of mapping the arguments in a function call to the function's
parameters. In addition to required parameters, a function can have
optional parameters. Or a function can have a single parameter that's
bound to a list containing any extra arguments. And, finally,
arguments can be mapped to parameters using keywords rather than
position. Thus, Common Lisp's parameter lists provide a convenient
solution to several common coding problems.

但是，Common Lisp
的形参列表也给了你更灵活的方式将函数调用实参映射到函数形参。除了必要形参以外，一个函数还可以有可选形参，或是也可以用单一形参绑定到含有任意多个额外参数的列表上。最后，参数还可以通过关键字而不是位置来映射到形参上。这样，Common
Lisp 的形参列表对于几种常见的编码问题提供了一种便利的解决方案。
