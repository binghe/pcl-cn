# Other Kinds of I/O（其他类型的 I/O）

In addition to file streams, Common Lisp supports other kinds of
streams, which can also be used with the various reading, writing, and
printing I/O functions. For instance, you can read data from, or write
data to, a string using **STRING-STREAM**s, which you can create with the
functions **MAKE-STRING-INPUT-STREAM** and **MAKE-STRING-OUTPUT-STREAM**.

除了文件流以外，Common Lisp
还支持其他类型的流，它们也可被用于各种读、写和打印 I/O
函数。例如，你可以使用 **STRING-STREAM**
从一个字符串中读取或写入数据，你可以使用函数 **MAKE-STRING-INPUT-STREAM**
和 **MAKE-STRING-OUTPUT-STREAM** 来创建 **STRING-STREAM**。

**MAKE-STRING-INPUT-STREAM** takes a string and optional start and end
indices to bound the area of the string from which data should be read
and returns a character stream that you can pass to any of the
character-based input functions such as **READ-CHAR**, **READ-LINE**, or
**READ**. For example, if you have a string containing a floating-point
literal in Common Lisp's syntax, you can convert it to a float like
this:

**MAKE-STRING-INPUT-STREAM**
接受一个字符串以及可选的开始和结尾指示符来鉴定字符串中数据应被读取的区域，然后返回一个可被传递到任何诸如
**READ-CHAR**、**READ-LINE** 或 **READ**
这些基于字符的输入函数中的字符流。例如，如果你有一个含有 Common
Lisp 语法的字面浮点数的字符串，那么你可以像下面这样将它转化成一个浮点数：

```lisp
(let ((s (make-string-input-stream "1.23")))
  (unwind-protect (read s)
    (close s)))
```

Similarly, **MAKE-STRING-OUTPUT-STREAM** creates a stream you can use with
**FORMAT**, **PRINT**, **WRITE-CHAR**, **WRITE-LINE**, and so on. It takes no
arguments. Whatever you write, a string output stream will be
accumulated into a string that can then be obtained with the function
**GET-OUTPUT-STREAM-STRING**. Each time you call **GET-OUTPUT-STREAM-STRING**,
the stream's internal string is cleared so you can reuse an existing
string output stream.

类似地，**MAKE-STRING-OUTPUT-STREAM** 创建一个流，其可被用于
**FORMAT**、**PRINT**、**WRITE-CHAR** 以及 **WRITE-LINE**
等。它不接受参数。无论你写了什么，字符串输出流都将被累积到字符串中，你随后可以通过函数
**GET-OUTPUT-STREAM-STRING**
来获取该字符串。每次当你调用 **GET-OUTPUT-STREAM-STRING**
时，该流的内部字符串会被清空，因此就可以重用一个已有的字符串输出流。

However, you'll rarely use these functions directly, because the
macros **WITH-INPUT-FROM-STRING** and **WITH-OUTPUT-TO-STRING** provide a more
convenient interface. **WITH-INPUT-FROM-STRING** is similar to
**WITH-OPEN-FILE**--it creates a string input stream from a given string
and then executes the forms in its body with the stream bound to the
variable you provide. For instance, instead of the LET form with the
explicit **UNWIND-PROTECT**, you'd probably write this:

不过你将很少直接使用这些函数，因为宏 **WITH-INPUT-FROM-STRING**
和 **WITH-OUTPUT-TO-STRING**
提供了一个更加便利的接口。**WITH-INPUT-FROM-STRING** 和
**WITH-OPEN-FILE**
相似，它从给定字符串中创建字符串输入流，并在该流绑定到你提供的变量的情况下执行它的主体的形式。例如，与其使用
**LET** 形式并带有显式的 **UNWIND-PROTECT**，你可以这样来写：

```lisp
(with-input-from-string (s "1.23")
  (read s))
```

The **WITH-OUTPUT-TO-STRING** macro is similar: it binds a newly created
string output stream to a variable you name and then executes its
body. After all the body forms have been executed,
**WITH-OUTPUT-TO-STRING** returns the value that would be returned by
**GET-OUTPUT-STREAM-STRING**.

**WITH-OUTPUT-TO-STRING**
与之相似：它把新创建的字符串输出流绑定到你所命名的变量上，然后执行它的主体。在所有主体形式都被执行以后，**WITH-OUTPUT-TO-STRING**
返回由 **GET-OUTPUT-STREAM-STRING** 所返回的值。

```lisp
CL-USER> (with-output-to-string (out)
            (format out "hello, world ")
            (format out "~s" (list 1 2 3)))
"hello, world (1 2 3)"
```

The other kinds of streams defined in the language standard provide
various kinds of stream "plumbing," allowing you to plug together
streams in almost any configuration. A **BROADCAST-STREAM** is an output
stream that sends any data written to it to a set of output streams
provided as arguments to its constructor function,
**MAKE-BROADCAST-STREAM**. Conversely, a **CONCATENATED-STREAM** is an input
stream that takes its input from a set of input streams, moving from
stream to stream as it hits the end of each
stream. **CONCATENATED-STREAM**s are constructed with the function
**MAKE-CONCATENATED-STREAM**, which takes any number of input streams as
arguments.

语言标准中定义的其他流提供了多种形式的流拼接技术，它允许你以几乎任何配置将流拼接在一起。**BROADCAST-STREAM**
是一个输出流，它将向其写入的任何数据发送到一组输出流上，这些流是作为参数提供给它的构造函数
**MAKE-BROADCAST-STREAM**
的。与之相反的，**CONCATENATED-STREAM**
是一个输入流，它从一组输入流中接收其输入，在每个流的结尾处它从一个流移动到另一个。你可以使用函数
**MAKE-CONCATENATED-STREAM** 来构造
**CONCATENATED-STREAM**，其接受任何数量的输入流作为参数。

Two kinds of bidirectional streams that can plug together streams in a
couple ways are **TWO-WAY-STREAM** and **ECHO-STREAM**. Their constructor
functions, **MAKE-TWO-WAY-STREAM** and **MAKE-ECHO-STREAM**, both take two
arguments, an input stream and an output stream, and return a stream
of the appropriate type, which you can use with both input and output
functions.

两种可以将流以多种方式拼接在一起的双向流是 **TWO-WAY-STREAM** 和 **ECHO-STREAM**。
它们的构造函数 **MAKE-TWO-WAY-STREAM** 和 **MAKE-ECHO-STREAM**
都接受两个参数，一个输入流和一个输出流，并返回一个适当类型的可同时用于输入和输出函数的流。

In a **TWO-WAY-STREAM** every read you perform will return data read from
the underlying input stream, and every write will send data to the
underlying output stream. An **ECHO-STREAM** works essentially the same
way except that all the data read from the underlying input stream is
also echoed to the output stream. Thus, the output stream of an
**ECHO-STREAM** stream will contain a transcript of both sides of the
conversation.

在 **TWO-WAY-STREAM**
中，你所做的每一次读取都会返回从底层输入流中读取的数据，而每次写入将把数据发送到底层的输出流上。除了所有从底层输入流中读取的数据也被回显到输出流中之外，**ECHO-STREAM**
本质上以相同的方式工作。这样，**ECHO-STREAM** 中的输出流将含有会话双方的一个副本。

Using these five kinds of streams, you can build almost any topology
of stream plumbing you want.

使用这五种流，你可以构造出几乎任何你想要的流拼接拓扑结构。

Finally, although the Common Lisp standard doesn't say anything about
networking APIs, most implementations support socket programming and
typically implement sockets as another kind of stream, so you can use
all the regular I/O functions with them.

最后，尽管 Common Lisp 标准并没有涉及有关网络 API
的内容，但多数实现都支持套接字（socket）编程并且通常将套接字实现成另一种类型的流，因此你可以使用常规
I/O 函数来操作它们。

Now you're ready to move on to building a library that smoothes over
some of the differences between how the basic pathname functions
behave in different Common Lisp implementations.

现在，你已准备好开始构建一个库来消除不同 Common Lisp
实现在基本路径名函数行为上的一些区别了。
