# Closing Files（关闭文件）

As anyone who has written code that deals with lots of files knows,
it's important to close files when you're done with them, because file
handles tend to be a scarce resource. If you open files and don't
close them, you'll soon discover you can't open any more files. It
might seem straightforward enough to just be sure every **OPEN** has a
matching **CLOSE**. For instance, you could always structure your file
using code like this:

任何编写过处理大量文件代码的人都知道，当处理完文件之后，关闭它们是多么重要。因为文件句柄往往是稀缺资源，如果打开一些文件却不关闭它们，你将很快发现不能再打开更多文件了。确保每一个
**OPEN** 都有一个匹配的 **CLOSE**
可能是非常显而易见的。例如，完全可以像下面这样来组织文件使用代码：

```lisp
(let ((stream (open "/some/file/name.txt")))
  ;; do stuff with stream
  (close stream))
```

However, this approach suffers from two problems. One is simply that
it's error prone--if you forget the **CLOSE**, the code will leak a file
handle every time it runs. The other--and more significant--problem is
that there's no guarantee you'll get to the **CLOSE**. For instance, if
the code prior to the **CLOSE** contains a **RETURN** or **RETURN-FROM**, you
could leave the **LET** without closing the stream. Or, as you'll see in
Chapter 19, if any of the code before the **CLOSE** signals an error,
control may jump out of the **LET** to an error handler and never come
back to close the stream.

但这一方法还是有两方面的问题。一是容易出现错误──如果忘记使用
**CLOSE**，那么代码将在每次运行时泄漏一个文件句柄。而更重要的一点是，于该代码并不保证你能够到达
**CLOSE** 那里。例如，如果 **CLOSE** 之前的代码含有一个 **RETURN**
或 **RETURN-FROM**，那就会在没有关闭流的情况下离开
**LET** 语句块。或者如同将第 19 章里将要介绍的，如果任何
**CLOSE** 之前的代码产生了一个错误，那么控制流可能就会跳出
**LET** 语句块而转到一个错误处理器中，然后不再回来关闭那个流。

Common Lisp provides a general solution to the problem of how to
ensure that certain code always runs: the special operator
**UNWIND-PROTECT**, which I'll discuss in Chapter 20. However, because the
pattern of opening a file, doing something with the resulting stream,
and then closing the stream is so common, Common Lisp provides a
macro, **WITH-OPEN-FILE**, built on top of **UNWIND-PROTECT**, to encapsulate
this pattern. This is the basic form:

Common Lisp
对于如何确保一直运行特定代码这一问题提供了一个通用的解决方案：特别操作符
**UNWIND-PROTECT**，第 20
章将予以讨论。不过因为这种打开文件、对产生的流做一些事情、然后再关闭流的模式是如此普遍，Common
Lisp 提供了一个构建在 **UNWIND-PROTECT**
之上的宏 **WITH-OPEN-FILE** 来封装这一模式。下面是它的基本形式：

```lisp
(with-open-file (stream-var open-argument*)
  body-form*)
```

The forms in `body-form`s are evaluated with stream-var bound to a file
stream opened by a call to **OPEN** with `open-argument`s as its
arguments. **WITH-OPEN-FILE** then ensures the stream in `stream-var` is
closed before the **WITH-OPEN-FILE** form returns. Thus, you can write
this to read a line from a file:

其中 `body-form` 中的形式将在
`stream-var` 被绑定到一个文件流的情况下进行求值，该流由一个对以
`open-argument` 为实参的 **OPEN**
调用而打开。**WITH-OPEN-FILE** 会确保 `stream-var`
中的流在 **WITH-OPEN-FILE**
返回之前被关闭。因为，从一个文件中读取一行的代码应如下所示：

```lisp
(with-open-file (stream "/some/file/name.txt")
  (format t "~a~%" (read-line stream)))
```

To create a new file, you can write something like this:

为了创建一个新文件，你可以写成像下面这样：

```lisp
(with-open-file (stream "/some/file/name.txt" :direction :output)
  (format stream "Some text."))
```

You'll probably use **WITH-OPEN-FILE** for 90-99 percent of the file I/O
you do--the only time you need to use raw **OPEN** and **CLOSE** calls is if
you need to open a file in a function and keep the stream around after
the function returns. In that case, you must take care to eventually
close the stream yourself, or you'll leak file descriptors and may
eventually end up unable to open any more files.

在你所使用的 90-99% 的文件 I/O 中都可能会用到
**WITH-OPEN-FILE**──需要使用原始 **OPEN** 和 **CLOSE**
调用的唯一情况是，当需要在一个函数中打开一个文件并在函数返回之后仍然保持所产生的流时。在那种情况下必须注意一点最终要由你自己来关闭这个流，否则你将会泄漏文件描述符并可能最终无法打开更多文件。

