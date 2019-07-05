# Experimenting in the REPL（体验 REPL）

To try the REPL, you need a Lisp expression that can be read, evaluated, and printed. One of the simplest kinds of Lisp expressions is a number. At the Lisp prompt, you can type `10` followed by Return and should see something like this:

为了测试 REPL，需要一个可以被读取、求值和打印的 Lisp 表达式。最简单类型的
Lisp 表达式是一个数。在 Lisp 提示符下，可以输入 `10`
接着敲回车键，然后看到类似下面的东西：

```
CL-USER> 10
10
```

The first `10` is the one you typed. The Lisp reader, the R in REPL, reads the text "10" and creates a Lisp object representing the number 10. This object is a self-evaluating object, which means that when given to the evaluator, the E in REPL, it evaluates to itself. This value is then given to the printer, which prints the 10 on the line by itself. While that may seem like a lot of work just to get back to where you started, things get a bit more interesting when you give Lisp something meatier to chew on. For instance, you can type `(+ 2 3)` at the Lisp prompt.

第一个 `10` 是你输入的。Lisp 读取器，即 REPL 中的 R，读取文本 “10”
并创建一个代表数字 10 的 Lisp
对象。这个对象是一个自求值（self-evaluating）对象，也就是说当把它送给求值器，即
REPL 中的 E 以后，它将对其自身求值。这个值随后被送到打印器里，打印出只有
10 的那行来。整个过程看起来似乎是费了九牛二虎之力却回到了原点，但如果你给了
Lisp 更有意义的信息，那么事情就变得有意思一些了。比如说，可以在 Lisp
提示符下输入 `(+ 2 3)`。

```
CL-USER> (+ 2 3)
5
```

Anything in parentheses is a list, in this case a list of three
elements, the symbol `+`, and the numbers 2 and 3. Lisp, in general,
evaluates lists by treating the first element as the name of a
function and the rest of the elements as expressions to be evaluated
to yield the arguments to the function. In this case, the symbol `+`
names a function that performs addition. 2 and 3 evaluate to
themselves and are then passed to the addition function, which
returns 5. The value 5 is passed to the printer, which prints it. Lisp
can evaluate a list expression in other ways, but we needn't get into
them right away. First we have to write...

小括号里的东西构成了一个列表，上述列表包括三个元素：符号 `+`，以及数字
2 和 3。一般来说，Lisp 对列表求值的时候会将第一个元素视为一个函数的名字，而其他元素作为即将求值的表达式则形成了该函数的实参。在本例里，符号
`+` 是加法函数的名字。2 和 3 对自身求值后被传递给加法函数，从而返回了
5。返回值 5 被传递给打印器从而得以输出。Lisp
也可能以其他方式对列表求值，但我们现在还没必要讨论它。但从 Hello World 开始。
