# "Hello, World," Lisp Style（Lisp 风格的 “Hello, World”）

No programming book is complete without a "hello, world" program. As
it turns out, it's trivially easy to get the REPL to print "hello,
world."

没有“hello，world”程序的编程书籍是不完整的。事实上，想让 REPL 打印出“hello, world”再简单不过了。

```
CL-USER> "hello, world"
"hello, world"
```

This works because strings, like numbers, have a literal syntax that's
understood by the Lisp reader and are self-evaluating objects: Lisp
reads the double-quoted string and instantiates a string object in
memory that, when evaluated, evaluates to itself and is then printed
in the same literal syntax. The quotation marks aren't part of the
string object in memory--they're just the syntax that tells the reader
to read a string. The printer puts them back on when it prints the
string because it tries to print objects in the same syntax the reader
understands.

其工作原理是，因为字符串和数字一样，带有 Lisp
读取器可以理解的字面语法并且是自求值对象：Lisp
读取双引号里的字符串，求值的时候在内存里建立一个可以对自身求值的字符串对象，然后再以同样的语法打印出来。双引号本身不是在内存中的字符串对象的一部分——它们只是语法，用来告诉读取器读入一个字符串。而打印器之所以在打印字符串时带上它们,则是因为其试图以一种读取器可以理解的相同语法来打印对象。

However, this may not really qualify as a "hello, world" program. It's
more like the "hello, world" value.

尽管如此，这还不能算是一个“hello, world”程序而更像是一个“hello, world”值。

You can take a step toward a real program by writing some code that as
a side effect prints the string "hello, world" to standard
output. Common Lisp provides a couple ways to emit output, but the
most flexible is the **FORMAT** function. **FORMAT** takes a variable number
of arguments, but the only two required arguments are the place to
send the output and a string. You'll see in the next chapter how the
string can contain embedded directives that allow you to interpolate
subsequent arguments into the string, àla `printf` or Python's
`string-%`. As long as the string doesn't contain an `~`, it will be
emitted as-is. If you pass t as its first argument, it sends its
output to standard output. So a **FORMAT** expression that will print
"hello, world" looks like this:

向真正的程序迈进一步的方法是编写一段代码，其副作用可以将字符串“hello,
world”打印到标准输出。Common Lisp 提供了许多产生输出的方法，但最灵活的是
**FORMAT** 函数。**FORMAT**
接收变长参数，但是只有两个必要的参数，分别代表着发送输出的位置以及字符串。在下一章里你将看到这个字符串是如何包含嵌入式指令，以便将其余的参数插入到字符串里的，就像
`printf` 或者 Python 的 `string-%` 那
样。只要字符串里不包含一个 `~`，那么它就会被原样输出。如果将 `t`
作为第一个参数传入，那么它将会发送其输出到标准输出。因此，一个将输出“hello,
world”的 **FORMAT** 表达式应如下所示：

```
CL-USER> (format t "hello, world")
hello, world
NIL
```

One thing to note about the result of the FORMAT expression is the NIL
on the line after the "hello, world" output. That NIL is the result of
evaluating the FORMAT expression, printed by the REPL. (NIL is Lisp's
version of false and/or null. More on that in Chapter 4.) Unlike the
other expressions we've seen so far, a FORMAT expression is more
interesting for its side effect--printing to standard output in this
case--than for its return value. But every expression in Lisp
evaluates to some result.

关于 **FORMAT**
表达式的结果，需要说明的一点是紧接着“hello, world”输出后面那行里的
**NIL**。那个 **NIL** 是 REPL 输出的求值 **FORMAT** 表达式的结果。（**NIL**
是 Lisp 版本的逻辑假和空值。更多内容见第 4
章。）和目前我们所见到的其他表达式不同的是，**FORMAT**
表达式的副作用（在本例中是打印到标准输出）比其返回值更有意义。但
Lisp 中的每个表达式都会求值出某些结果。

However, it's still arguable whether you've yet written a true
"program." But you're getting there. And you're seeing the bottom-up
style of programming supported by the REPL: you can experiment with
different approaches and build a solution from parts you've already
tested. Now that you have a simple expression that does what you want,
you just need to package it in a function. Functions are one of the
basic program building blocks in Lisp and can be defined with a DEFUN
expression such as this:

尽管如此，你是否已经写出了一个真正的“程序”呢？恐怕仍有争议。不过你离目标越来越近了。而且你正在体会
REPL 所带来的自底向上的编程风格：可以试验不同的方法，然后从已经测试过的部分里构建出一个解决方案来。现在你已经写出了一个简单的表达式来做你想要的事，剩下的就是将其打包成一个函数了。函数是
Lisp 的基本程序构造单元，可以用类似下面这样的 **DEFUN** 表达式来定义：

```
CL-USER> (defun hello-world () (format t "hello, world"))
HELLO-WORLD
```

The `hello-world` after the **DEFUN** is the name of the function. In
Chapter 4 we'll look at exactly what characters can be used in a name,
but for now suffice it to say that lots of characters, such as `-`, that
are illegal in names in other languages are legal in Common Lisp. It's
standard Lisp style--not to mention more in line with normal English
typography--to form compound names with hyphens, such as `hello-world`,
rather than with underscores, as in `hello_world`, or with inner caps
such as `helloWorld`. The `()`s after the name delimit the parameter list,
which is empty in this case because the function takes no
arguments. The rest is the body of the function.

**DEFUN** 后面的 `hello-world` 是这个函数的名字。在第 4
章里我们将看到究竟哪些字符可以在名字里使用，现在我们暂时假设包括 `-`
在内的很多在其他语言里非法的字符在 Common Lisp 里都是合法的。像
`hello-world` 这种用连字符而不是下划线（`hello_world`）或是内部大写（`helloWorld`）来形成复合词的方法，是标准的
Lisp 风格——更不用提那接近正常英语的排版了。名字后面的 `()` 是形参列表，
在本例中为空是因为该函数不带参数。其余的部分是函数体。

At one level, this expression, like all the others you've seen, is
just another expression to be read, evaluated, and printed by the
REPL. The return value in this case is the name of the function you
just defined. But like the **FORMAT** expression, this expression is
more interesting for the side effects it has than for its return
value. Unlike the **FORMAT** expression, however, the side effects are
invisible: when this expression is evaluated, a new function that
takes no arguments and with the body `(format t "hello, world")` is
created and given the name **HELLO-WORLD**.

表面上看，这个表达式和你目前见到的所有其他表达式一样，只是另一个被 REPL
读取、求值和打印的表达式。这里的返回值是你所定义的函数名。 但是和
**FORMAT** 表达式一样，这个表达式的副作用比其返回值更有用。但与
**FORMAT** 表达式所不同的是，它的副作用是不可见的：当这个表达式被求值的时候，一个不带参数且函数体为
`(format t "hello, world")` 的新函数会被创建出来并被命名为
**HELLO-WORLD**。

Once you've defined the function, you can call it like this:

一旦定义了这个函数，你就可以像这样来调用它：

```
CL-USER> (hello-world)
hello, world
NIL
```

You can see that the output is just the same as when you evaluated the
**FORMAT** expression directly, including the **NIL** value printed by the
REPL. Functions in Common Lisp automatically return the value of the
last expression evaluated.

你将看到输出和直接对 **FORMAT** 表达式求值时是一样的，包括 REPL 打印出的
**NIL** 值。Common Lisp 中的函数自动返回其最后求值的那个表达式的值。
