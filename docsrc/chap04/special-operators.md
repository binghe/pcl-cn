# Special Operators（特别操作符）

That said, not all operations can be defined as functions. Because all
the arguments to a function are evaluated before the function is
called, there's no way to write a function that behaves like the IF
operator you used in Chapter 3. To see why, consider this form:

然而，并非所有的操作都可被定义成函数。由于一个函数的所有参数在函数被调用之前都将被求值，因此无法写出一个类似第 3
章里用到的 **IF** 操作符那样的函数。为了说明这点，可以假设有下面这种形式：

```lisp
(if x (format t "yes") (format t "no"))
```

If IF were a function, the evaluator would evaluate the argument
expressions from left to right. The symbol `x` would be evaluated as a
variable yielding some value; then `(format t "yes")` would be evaluated
as a function call, yielding **NIL** after printing "yes" to standard
output. Then `(format t "no")` would be evaluated, printing "no" and
also yielding **NIL**. Only after all three expressions were evaluated
would the resulting values be passed to IF, too late for it to control
which of the two **FORMAT** expressions gets evaluated.

如果 **IF** 是一个函数，那么求值器将从左到右依次对其参数表达式求值。符号
`x` 将被作为产生某个值的变量来求值，然后 `(format t "yes")`
将被当成一个函数调用来求值，在向标准输出打印 “yes” 以后得到
**NIL**。接下来 `(format t "no")` 将被求值，打印出 “no” 同时也得到
**NIL**。只有当所有三个表达式都被求值以后，它们的结果值才被传递给
**IF**，而这时已经无法控制两个 **FORMAT** 表达式中的哪一个会被求值了。

To solve this problem, Common Lisp defines a couple dozen so-called
special operators, **IF** being one, that do things that functions can't
do. There are 25 in all, but only a small handful are used directly in
day-to-day programming.

为了解决这个问题，Common Lisp 定义了一些特别操作符，**IF**
就是其中之一，它们可以做到函数无法做到的事情。它们总共有 25
个，但只有很少一部分直接用于日常编程。

When the first element of a list is a symbol naming a special
operator, the rest of the expressions are evaluated according to the
rule for that operator.

当列表的第一个元素是一个由特别操作符所命名的符号时，表达式的其余部分将按照该操作符的规则进行求值。

The rule for **IF** is pretty easy: evaluate the first expression. If it
evaluates to non-**NIL**, then evaluate the next expression and return its
value. Otherwise, return the value of evaluating the third expression
or **NIL** if the third expression is omitted. In other words, the basic
form of an IF expression is as follows:

**IF** 的规则相当简单：求值第一个表达式。如果得到非
**NIL**，那么求值下一个表达式并返回它的值。否则，返回第三个表达式的求值，或者
**NIL** 如果第三个表达式被省略的话，返回 **NIL**。换句话说，一个
**IF** 表达式的基本形式是像下面这样：

```lisp
(if test-form then-form [ else-form ])
```

The `test-form` will always be evaluated and then one or the other of
the `then-form` or `else-form`.

其中 `test-form` 将总是被求值，然后要么是 `then-form` 要么是 `else-form`。

An even simpler special operator is **QUOTE**, which takes a single
expression as its "argument" and simply returns it, unevaluated. For
instance, the following evaluates to the list `(+ 1 2)`, not the value
3:

一个更简单的特别操作符是 **QUOTE**，它接受一个单一表达式作为其 “参数”
并简单地返回它，不经求值。例如，下面的表达式求值得到列表 `(+ 1 2)`，而不是值 3:

```lisp
(quote (+ 1 2))
```

There's nothing special about this list; you can manipulate it just
like any list you could create with the **LIST** function.

这个列表没有什么特别的，你可以像用 **LIST** 函数所创建的任何列表那样处理它。

**QUOTE** is used commonly enough that a special syntax for it is built
into the reader. Instead of writing the following:

**QUOTE**
被用得相当普遍，以至于读取中内置了一个它的特别语法形式。除了能像下面这样写之外：

```lisp
(quote (+ 1 2))
```

you can write this:

你也可以这样写：

```lisp
'(+ 1 2)
```

This syntax is a small extension of the s-expression syntax understood
by the reader. From the point of view of the evaluator, both those
expressions will look the same: a list whose first element is the
symbol **QUOTE** and whose second element is the list `(+ 1 2)`.

该语法是读取器所理解的
S-表达式语法的小扩展。从求值器的观点来看，这两个表达式看起来是一样的：一个首元素为符号
**QUOTE** 并且次元素是列表 `(+ 1 2)` 的列表。

In general, the special operators implement features of the language
that require some special processing by the evaluator. For instance,
several special operators manipulate the environment in which other
forms will be evaluated. One of these, which I'll discuss in detail in
Chapter 6, is **LET**, which is used to create new variable bindings. The
following form evaluates to 10 because the second x is evaluated in an
environment where it's the name of a variable established by the **LET**
with the value 10:

一般来说，特别操作符所实现的语言特性需要求值器作出某些特殊处理。例如，有些的操作符修改了其他形式将被求值环境。其中之一是
**LET**，也是我将在第 6 章详细讨论的，它用来创建新的变量绑定。下面的形式求值得到
10，因为在第二个 `x` 的求值环境中，它由 **LET** 赋值为 10 的变量名：

```lisp
(let ((x 10)) x)
```
