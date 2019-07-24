# Reading File Data（读写文件数据）

The most basic file I/O task is to read the contents of a file. You
obtain a stream from which you can read a file's contents with the
**OPEN** function. By default **OPEN** returns a character-based input stream
you can pass to a variety of functions that read one or more
characters of text: **READ-CHAR** reads a single character; **READ-LINE**
reads a line of text, returning it as a string with the end-of-line
character(s) removed; and **READ** reads a single s-expression, returning
a Lisp object. When you're done with the stream, you can close it with
the **CLOSE** function.

最基本的文件 I/O 任务是读取文件的内容。可以通过 **OPEN**
函数获得一个流并从中读取文件的内容。默认情况下，**OPEN**
返回一个基于字符的输入流，你可以将它传给许多函数以便读取文本中的一个或多个字符：**READ-CHAR**
读取单个字符；**READ-LINE**
读取一行文本，去掉行结束字符后作为一个字符串返回；而 **READ**
读取单一的 S-表达式并返回一个 Lisp
对象。当完成了对流的操作后，你可以使用 **CLOSE** 函数来关闭它。

The only required argument to **OPEN** is the name of the file to read. As
you'll see in the section "Filenames," Common Lisp provides a couple
of ways to represent a filename, but the simplest is to use a string
containing the name in the local file-naming syntax. So assuming that
`/some/file/name.txt` is a file, you can open it like this:

**OPEN** 的唯一必要参数是需要读取的文件名。如同你将会在 14.6
节里看到的那样，Common Lisp
提供了许多表示文件名的方式，但最简单的方式是使用一个含有以本地文件命名语法表示的文件名的字符串。因此，假设
`/some/file/name.txt` 是一个文件，那么可以像下面这样打开它：

```lisp
(open "/some/file/name.txt")
```

You can use the object returned as the first argument to any of the
read functions. For instance, to print the first line of the file, you
can combine **OPEN**, **READ-LINE**, and **CLOSE** as follows:

你可以把返回对象作为任何读取函数的第一个参数。例如，为了打印文件的第一行，你可以组合使用
**OPEN**、**READ-LINE** 和 **CLOSE**，如下所示：

```lisp
(let ((in (open "/some/file/name.txt")))
  (format t "~a~%" (read-line in))
  (close in))
```

Of course, a number of things can go wrong while trying to open and
read from a file. The file may not exist. Or you may unexpectedly hit
the end of the file while reading. By default **OPEN** and the `READ-*`
functions will signal an error in these situations. In Chapter 19,
I'll discuss how to recover from such errors. For now, however,
there's a lighter-weight solution: each of these functions accepts
arguments that modify its behavior in these exceptional situations.

当然，在试图打开和读取一个文件时可能会出现一些错误。该文件可能不存在或
者可能在读取时无意中遇到了文件结尾。默认情况下，**OPEN** 和 `READ-*`
系列函数将在出现这些情况下时报错。在第 19
章里，我将讨论如何从这类错误中恢复。不过眼下有一个更轻量级的解决方案：每个这样的函数都可接受参数来修改这些异常情况下的行为。

If you want to open a possibly nonexistent file without **OPEN** signaling
an error, you can use the keyword argument `:if-does-not-exist` to
specify a different behavior. The three possible values are `:error`,
the default; `:create`, which tells it to go ahead and create the file
and then proceed as if it had already existed; and **NIL**, which tells it
to return **NIL** instead of a stream. Thus, you can change the previous
example to deal with the possibility that the file may not exist.

如果你想打开一个可能不存在的文件而又不想让 **OPEN**
报错，那么可以使用关键字参数 `:if-does-not-exist`
来指定一个不同的行为。三个可能的值是：`:error`，报错（默认值）；`:create`，继续进行并创建该文件，然后就像它已经存在那样进行处理；**NIL**，让它返回
**NIL** 来代替一个流。这样，你就可以改变前面的示例来处理文件可能不存在的情况。

```lisp
(let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))
```

The reading functions--**READ-CHAR**, **READ-LINE**, and **READ**--all take an
optional argument, which defaults to true, that specifies whether they
should signal an error if they're called at the end of the file. If
that argument is **NIL**, they instead return the value of their third
argument, which defaults to **NIL**. Thus, you could print all the lines
in a file like this:

读取函数，**READ-CHAR**、**READ-LINE** 和
**READ**，都接受一个可选的参数，默认值为真并指定当函数在文件结尾处被调用时是否应该报错。如果该参数为
**NIL**，它们在遇到文件结尾时将返回它们第三个参数的值，默认为
**NIL**。因此，可以像下面这样打印一个文件的所有行：

```lisp
(let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))
```

Of the three text-reading functions, **READ** is unique to Lisp. This is
the same function that provides the *R* in the REPL and that's used to
read Lisp source code. Each time it's called, it reads a single
s-expression, skipping whitespace and comments, and returns the Lisp
object denoted by the s-expression. For instance, suppose
`/some/file/name.txt` has the following contents:

在这三个文本读取函数中，**READ** 是 Lisp 独有的。这与提供了 REPL 中*R*
部分的函数是同一个，它被用于读取 Lisp 源代码。当每次被调用时，它会读取单一的
S-表达式，跳过空格和注释，然后返回由 S-表达式代表的 Lisp
对象。例如，假设 `/some/file/name.txt` 带有下列内容：

```lisp
(1 2 3)
456
"a string" ; this is a comment
((a b)
 (c d))
```

In other words, it contains four s-expressions: a list of numbers, a
number, a string, and a list of lists. You can read those expressions
like this:

换句话说，它含有四个
S-表达式：一个数字列表、一个数字、一个字符串和一个列表的列表。你可以像下面这样读取这些表达式：

```lisp
CL-USER> (defparameter *s* (open "/some/file/name.txt"))
*S*
CL-USER> (read *s*)
(1 2 3)
CL-USER> (read *s*)
456
CL-USER> (read *s*)
"a string"
CL-USER> (read *s*)
((A B) (C D))
CL-USER> (close *s*)
T
```

As you saw in Chapter 3, you can use **PRINT** to print Lisp objects in
"readable" form. Thus, whenever you need to store a bit of data in a
file, **PRINT** and **READ** provide an easy way to do it without having to
design a data format or write a parser. They even--as the previous
example demonstrated--give you comments for free. And because
s-expressions were designed to be human editable, it's also a fine
format for things like configuration files.

如同第 3 章所述，你可以使用 **PRINT** 以 “可读的” 形式打印 Lisp
对象。这样，每当你需要在文件中保存一点数据时，**PRINT** 和 **READ**
就提供了一个做这件事的简单途径，而无需设计一套数据格式或编写一个解析器。它们甚至可以让你自由地添加注释，如同前面的示例所演示的那样。并且由于
S-表达式被设计成是可人工编辑的，所以它也是用于诸如配置文件等事务的良好格式。
