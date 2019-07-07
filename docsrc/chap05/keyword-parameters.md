# Keyword Parameters（关键字形参）

Optional and rest parameters give you quite a bit of flexibility, but
neither is going to help you out much in the following situation:
Suppose you have a function that takes four optional parameters. Now
suppose that most of the places the function is called, the caller
wants to provide a value for only one of the four parameters and,
further, that the callers are evenly divided as to which parameter
they will use.

尽管可选形象和剩余形参带来了很大的灵活性，但两者却都不能帮助应对下面的情形。假设有一个接受四个可选形参的函数，如果在多数函数的调用中，调用者只想为四个参数中的一个提供值，并且更进一步，不同的调用者甚至可能将分别选择使用其中一个参数。

The callers who want to provide a value for the first parameter are
fine--they just pass the one optional argument and leave off the
rest. But all the other callers have to pass some value for between
one and three arguments they don't care about. Isn't that exactly the
problem optional parameters were designed to solve?

想为第一个形参提供值的调用者将会很方便——只需传递一个可选实参，然后忽略其他就好了。但是所有其他的调用者将不得不为所不关心的一到三个形参传递一些值。这不正是可选形参想来解决的问题吗？

Of course it is. The problem is that optional parameters are still
positional--if the caller wants to pass an explicit value for the
fourth optional parameter, it turns the first three optional
parameters into required parameters for that caller. Luckily, another
parameter flavor, keyword parameters, allow the caller to specify
which values go with which parameters.

当然是。问题在于可选形参仍然是位置相关的——如果调用者想要给第四个可选形参传递一个显式的值，就会导致前三个可选形参对于该调用者来说变成了必要形参。幸好我们有另一种形参类型，关键字形参，它可以允许调用者指定具体形参相应所使用的值。

To give a function keyword parameters, after any required, `&optional`,
and `&rest` parameters you include the symbol `&key` and then any number
of keyword parameter specifiers, which work like optional parameter
specifiers. Here's a function that has only keyword parameters:

为了使函数带有关键字形参，在任何必要的、`&optional` 和 `&rest`
形参之后，可以加上符号 `&key`
以及任意数量的关键字形参标识符，后者的格式类似于可选形参标识符。下面就是一个只有关键字形参的函数：

```lisp
(defun foo (&key a b c) (list a b c))
```

When this function is called, each keyword parameters is bound to the
value immediately following a keyword of the same name. Recall from
Chapter 4 that keywords are names that start with a colon and that
they're automatically defined as self-evaluating constants.

当调用这个函数时，每一个关键字形参将被绑定到紧跟在同名键字后面的那个值上。如第
4 章所述，关键字是以冒号开始的名字，并且它们被自动定义为自求值常量。

If a given keyword doesn't appear in the argument list, then the
corresponding parameter is assigned its default value, just like an
optional parameter. Because the keyword arguments are labeled, they
can be passed in any order as long as they follow any required
arguments. For instance, foo can be invoked as follows:

如果一个给定的关键字没有出现在实参列表中，那么对应的形参将被赋予其默认值，如同可选形参那样。因为关键字实参带有标签，所以它们在必要实参之后可按任意顺序进行传递。例如
`foo` 可以用下列形式调用：

```lisp
(foo)                ==> (NIL NIL NIL)
(foo :a 1)           ==> (1 NIL NIL)
(foo :b 1)           ==> (NIL 1 NIL)
(foo :c 1)           ==> (NIL NIL 1)
(foo :a 1 :c 3)      ==> (1 NIL 3)
(foo :a 1 :b 2 :c 3) ==> (1 2 3)
(foo :a 1 :c 3 :b 2) ==> (1 2 3)
```

As with optional parameters, keyword parameters can provide a default
value form and the name of a `supplied-p` variable. In both keyword and
optional parameters, the default value form can refer to parameters
that appear earlier in the parameter list.

如同可选形参那样，关键字形参也可以提供一个默认值形式以及一个
`supplied-p` 变量名。在关键字形参和可选形参中，这个默认值形式都可以引用那些早先出现在形参列表中的形参。

```lisp
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo :a 1)           ==> (1 0 1 NIL)
(foo :b 1)           ==> (0 1 1 T)
(foo :b 1 :c 4)      ==> (0 1 4 T)
(foo :a 2 :b 1 :c 4) ==> (2 1 4 T)
```

Also, if for some reason you want the keyword the caller uses to
specify the parameter to be different from the name of the actual
parameter, you can replace the parameter name with another list
containing the keyword to use when calling the function and the name
to be used for the parameter. The following definition of foo:

同样，如果出于某种原因想让调用者用来指定形参的关键字不同于实际形参名，那么可以将形参名替换成一个列表，令其含有调用函数时使用的关键字以及用作形参的名字。比如说下面这个
`foo` 的定义：

```lisp
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
```

lets the caller call it like this:

可以让调用者这样调用它：

```lisp
(foo :apple 10 :box 20 :charlie 30) ==> (10 20 30 T)
```

This style is mostly useful if you want to completely decouple the
public API of the function from the internal details, usually because
you want to use short variable names internally but descriptive
keywords in the API. It's not, however, very frequently used.

这种风格在想要完全将函数的公共 API
与其内部细节相隔离时特别有用，通常是因为想要在内部使用短变量名，而不是
API 中的描述性关键字。不过该特性不常被用到。
