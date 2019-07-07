# Rest Parameters（剩余形参）

Optional parameters are just the thing when you have discrete
parameters for which the caller may or may not want to provide
values. But some functions need to take a variable number of
arguments. Several of the built-in functions you've seen already work
this way. **FORMAT** has two required arguments, the stream and the
control string. But after that it needs a variable number of arguments
depending on how many values need to be interpolated into the control
string. The `+` function also takes a variable number of
arguments--there's no particular reason to limit it to summing just
two numbers; it will sum any number of values. (It even works with
zero arguments, returning 0, the identity under addition.) The
following are all legal calls of those two functions:

可选形参仅适用于一些较为分散并且不能确定调用者是否会供值的形参。但某些函数需要接收可变数量的实参，比如说前文已然出现过的一些内置函数。**FORMAT**
有两个必要实参，即流和控制串。但在这两个之后，它还需要一组可变数量的实参，这取决于控制串需要插入多少个值。`+`
函数也接受可变数量的实参——没有特别的理由限制它只能在两个数之间相加，它对任意数量的值做加法运算（它甚至可以没有实参，此时返回
0——加法的底数）。下面这些都是这两个函数的合法调用：

```lisp
(format t "hello, world")
(format t "hello, ~a" name)
(format t "x: ~d y: ~d" x y)
(+)
(+ 1)
(+ 1 2)
(+ 1 2 3)
```

Obviously, you could write functions taking a variable number of
arguments by simply giving them a lot of optional parameters. But that
would be incredibly painful--just writing the parameter list would be
bad enough, and that doesn't get into dealing with all the parameters
in the body of the function. To do it properly, you'd have to have as
many optional parameters as the number of arguments that can legally
be passed in a function call. This number is implementation dependent
but guaranteed to be at least 50. And in current implementations it
ranges from 4,096 to 536,870,911. Blech. That kind of mind-bending
tedium is definitely not The Lisp Way.

很明显，也可以通过简单地给它一些可选形参来写出接受可变数量实参的函数，但这样将会非常麻烦——光是写形参列表就已经足够麻烦了，何况还要在函数体中处理所有这些形参。为了做好这件事，还将不得不使用一个合法的函数调用所能够传递的那么多的可选形参。这一具体数量与具体实现相关，但可以保证至少有
50 个。在当前所有实现中，它的最大值范围从 4096 到
536,870,911。这种绞尽脑汁的无聊事情绝对不是 Lisp 风格。

Instead, Lisp lets you include a catchall parameter after the symbol
`&rest`. If a function includes a `&rest` parameter, any arguments
remaining after values have been doled out to all the required and
optional parameters are gathered up into a list that becomes the value
of the `&rest` parameter. Thus, the parameter lists for **FORMAT** and `+`
probably look something like this:

相反，Lisp 允许在符号 `&rest` 之后包括一揽子形参。如果函数带有 `&rest`
形参，那么任何满足了必要和可选形参之后的其余所有实参就将被收集到一个列表里成为该
`&rest` 形参的值。这样，**FORMAT** 和 `+` 的形参列表可能看起来会是这样：

```lisp
(defun format (stream string &rest values) ...)
(defun + (&rest numbers) ...)
```
