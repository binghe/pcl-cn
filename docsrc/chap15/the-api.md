# The API（应用程序接口）

The basic operations the library will support will be getting a list
of files in a directory and determining whether a file or directory
with a given name exists. You'll also write a function for recursively
walking a directory hierarchy, calling a given function for each
pathname in the tree.

该库支持的基本操作是获取目录中的文件列表，并检测给定名字的文件或目录是否存在。你也将编写函数用于递归遍历目录层次，并在目录树的每个路径名上调用给定的函数。

In theory, these directory listing and file existence operations are
already provided by the standard functions **DIRECTORY** and
**PROBE-FILE**. However, as you'll see, there are enough different ways to
implement these functions--all within the bounds of valid
interpretations of the language standard--that you'll want to write
new functions that provide a consistent behavior across
implementations.

从理论上来讲，这些列目录和测试文件存在性的操作已经由标准函数 **DIRECTORY**
和 **PROBE-FILE**
提供了。不过正如你将看到的那样，会有许多不同的方式来实现这些函数──所有这些均属于语言标准的有效解释，因此你希望编写新的函数，以在不同实现间提供一致的行为。
