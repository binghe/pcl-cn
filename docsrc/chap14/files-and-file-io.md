# Files and File I/O（文件和文件 I/O）

Common Lisp provides a rich library of functionality for dealing with
files. In this chapter I'll focus on a few basic file-related tasks:
reading and writing files and listing files in the file system. For
these basic tasks, Common Lisp's I/O facilities are similar to those
in other languages. Common Lisp provides a stream abstraction for
reading and writing data and an abstraction, called pathnames, for
manipulating filenames in an operating system-independent
way. Additionally, Common Lisp provides other bits of functionality
unique to Lisp such as the ability to read and write s-expressions.

Common Lisp
提供了一个功能丰富用于处理文件的函数库。在本章里，我将把重点放在少数基本的文件相关任务上：读写文件以及列出文件系统中的文件。对这些基本任务，Common
Lisp 的输入输出（I/O）机制与其他语言相似。Common
Lisp 为读写数据提供了一个流的抽象和一个称为路径名（pathname）的抽象，
它们以一种与操作系统无关的方式来管理文件名。另外，Common Lisp
还提供了其他一些 Lisp 独有的功能，比如读写 S-表达式的能力。

