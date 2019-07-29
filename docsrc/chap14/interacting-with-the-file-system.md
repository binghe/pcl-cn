# Interacting with the File System（与文件系统交互）

While the most common interaction with the file system is probably
**OPEN**ing files for reading and writing, you'll also occasionally want
to test whether a file exists, list the contents of a directory,
delete and rename files, create directories, and get information about
a file such as who owns it, when it was last modified, and its
length. This is where the generality of the pathname abstraction
begins to cause a bit of pain: because the language standard doesn't
specify how functions that interact with the file system map to any
specific file system, implementers are left with a fair bit of leeway.

尽管最常见的文件系统交互可能是用 **OPEN**
打开文件来读写，你偶尔也需要测试一个文件是否存在，列出一个目录的内容，删除和重命名文件，创建目录以及获取一个文件的信息，诸如谁拥有它、何时被最后修改以及它的长度。这就是由路径名抽象所带来的通用性开始造成痛苦的地方：因为语言标准并没有指定那些与文件系统交互的函数是如何映射到任何特定的文件系统上的，从而给实现者们留下了充分的余地。

That said, most of the functions that interact with the file system
are still pretty straightforward. I'll discuss the standard functions
here and point out the ones that suffer from nonportability between
implementations. In the next chapter you'll develop a pathname
portability library to smooth over some of those nonportability
issues.

这就是说，多数与文件系统进行交互的函数仍然是相当直接的。我将在这里讨论标准函数，并且指出其中哪些是在实现之间存在不可移植性的。在下一章里，你将开发一个路径名可移植库来消除那些不可移植因素中的一部分。

To test whether a file exists in the file system corresponding to a
pathname designator--a pathname, namestring, or file stream--you can
use the function **PROBE-FILE**. If the file named by the pathname
designator exists, **PROBE-FILE** returns the file's truename, a pathname
with any file system-level translations such as resolving symbolic
links performed. Otherwise, it returns **NIL**. However, not all
implementations support using this function to test whether a
directory exists. Also, Common Lisp doesn't provide a portable way to
test whether a given file that exists is a regular file or a
directory. In the next chapter you'll wrap **PROBE-FILE** with a new
function, `file-exists-p`, that can both test whether a directory exists
and tell you whether a given name is the name of a file or directory.

为了测试一个对应于某个路径名描述符（路径名、名字字符串或文件流）的文件是否存在于文件系统中，你可以使用函数
**PROBE-FILE**。如果由路径名描述符命名的文件存在，那么
**PROBE-FILE**
将返回该文件的真实名称（truename），一个将诸如解析符号链接这类文件系统层面转换进行过的路径名。否则它返回
**NIL**。不过，并非所有实现都支持使用该函数来测试一个目录是否存在。同样，Common
Lisp 也不支持用一种可移植的方式来测试一个给定文件是否作为正规文件或目录而存在。在下一章里，你将把
**PROBE-FILE** 包装在一个新函数 `file-exists-p`
中，它不但可以测试目录是否存在，还可以告诉你一个给定的名字究竟是文件名还是目录名。

Similarly, the standard function for listing files in the file system,
**DIRECTORY**, works fine for simple cases, but the differences between
implementations make it tricky to use portably. In the next chapter
you'll define a list-directory function that smoothes over some of
these differences.

类似地，用于列出文件系统中文件的标准函数 **DIRECTORY**
对于简单的情形工作得很好，但实现之间的区别却使得它难以可移植地使用。在下一章里，你将定义一个
`list-directory` 函数来消除这些区别。

**DELETE-FILE** and **RENAME-FILE** do what their names suggest. **DELETE-FILE**
takes a pathname designator and deletes the named file, returning true
if it succeeds. Otherwise it signals a **FILE-ERROR**.

**DELETE-FILE** 和 **RENAME-FILE** 的功能恰如其名。**DELETE-FILE**
接受一个路径名描述符并删除所命名的文件，当其成功时返回真。否则它产生一个
**FILE-ERROR** 报错。

**RENAME-FILE** takes two pathname designators and renames the file named
by the first name to the second name.

**RENAME-FILE** 接受两个路径名描述符，并将第一个名字命名的文件重命名为第二个名字。

You can create directories with the function
**ENSURE-DIRECTORIES-EXIST**. It takes a pathname designator and ensures
that all the elements of the directory component exist and are
directories, creating them as necessary. It returns the pathname it
was passed, which makes it convenient to use inline.

你可以使用函数 **ENSURE-DIRECTORIES-EXIST**
来创建目录。它接受一个路径名描述符并确保目录组件中的所有元素存在并且是目录，如果必要的话它会创建它们。它返回被传递的路径名，这使得它易于内联使用。

```lisp
(with-open-file (out (ensure-directories-exist name) :direction :output)
   ...
   )
```

Note that if you pass **ENSURE-DIRECTORIES-EXIST** a directory name, it
should be in directory form, or the leaf directory won't be created.

注意，如果你传给 **ENSURE-DIRECTORIES-EXIST**
一个目录名，它应该是目录形式的，否则目录的最后一级子目录将不会被创建。

The functions **FILE-WRITE-DATE** and **FILE-AUTHOR** both take a pathname
designator. **FILE-WRITE-DATE** returns the time in number of seconds
since midnight January 1, 1900, Greenwich mean time (GMT), that the
file was last written, and **FILE-AUTHOR** returns, on Unix and Windows,
the file owner.

函数 **FILE-WRITE-DATE** 和 **FILE-AUTHOR**
都接受一个路径名描述符。**FILE-WRITE-DATE**
返回文件上次被写入的时间，表示形式是自从格林尼治标准时间（GMT）
1900 年 1 月 1 日午夜起的秒数，而 **FILE-AUTHOR**
在 Unix 和 Windows 上返回该文件的拥有者。

To find the length of a file, you can use the function
**FILE-LENGTH**. For historical reasons **FILE-LENGTH** takes a stream as an
argument rather than a pathname. In theory this allows **FILE-LENGTH** to
return the length in terms of the element type of the stream. However,
since on most present-day operating systems, the only information
available about the length of a file, short of actually reading the
whole file to measure it, is its length in bytes, that's what most
implementations return, even when **FILE-LENGTH** is passed a character
stream. However, the standard doesn't require this behavior, so for
predictable results, the best way to get the length of a file is to
use a binary stream.

为了知道一个文件的长度，你可以使用函数
**FILE-LENGTH**。出于历史原因，**FILE-LENGTH**
接受一个流而不是一个路径名作为参数。在理论上，这允许
**FILE-LENGTH**
返回在该流的元素类型意义下的长度。尽管如此，由于在当今大多数操作系统上关于一个文件的长度唯一可以得到的信息（除了实际读取整个文件来测量它以外）只有以字节为单位的长度，这也是多数实现所返回的，甚至当
**FILE-LENGTH**
被传递了一个字符流时情况也是如此。不过，标准并没有强制要求这一行为，因此为了得到可预测的结果，获得一个文件长度的最佳方式是使用一个二进制流。

```lisp
(with-open-file (in filename :element-type '(unsigned-byte 8))
  (file-length in))
```

A related function that also takes an open file stream as its argument
is **FILE-POSITION**. When called with just a stream, this function
returns the current position in the file--the number of elements that
have been read from or written to the stream. When called with two
arguments, the stream and a position designator, it sets the position
of the stream to the designated position. The position designator must
be the keyword `:start`, the keyword `:end`, or a non-negative
integer. The two keywords set the position of the stream to the start
or end of the file while an integer moves to the indicated position in
the file. With a binary stream the position is simply a byte offset
into the file. However, for character streams things are a bit more
complicated because of character-encoding issues. Your best bet, if
you need to jump around within a file of textual data, is to only ever
pass, as a second argument to the two-argument version of
**FILE-POSITION**, a value previously returned by the one-argument version
of **FILE-POSITION** with the same stream argument.

一个同样接受打开的文件流作为参数的相关函数是
**FILE-POSITION**。当只用一个流来调用时，该函数返回文件中的当前位置，即已经被读取或写入该流的元素的数量。当以两个参数（流和位置描述符）来调用该函数时，它将该流的位置设置到所描述的位置上。这个位置描述符必须是关键字
`:start`、`:end`
或者非负的整数。两个关键字可以将流的位置设置到文件的开始或结尾处，而一个整数将使流的位置移动到文件中指定的位置上。对于二进制流来说，这个位置就是文件中的字节偏移量。尽管如此，因为字符编码因素的存在，对于字符流来说事情变得有一点儿复杂。当你需要在一个文本数据的文件中做跳转时，最可靠的方法就是只为两参数版本的
**FILE-POSITION**
的第二个参数传递一个由单参数版本
**FILE-POSITION** 同样的流参数下曾经返回的一个值。
