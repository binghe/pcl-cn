# Files and File I/O

Common Lisp provides a rich library of functionality for dealing with
files. In this chapter I'll focus on a few basic file-related tasks:
reading and writing files and listing files in the file system. For
these basic tasks, Common Lisp's I/O facilities are similar to those
in other languages. Common Lisp provides a stream abstraction for
reading and writing data and an abstraction, called pathnames, for
manipulating filenames in an operating system-independent
way. Additionally, Common Lisp provides other bits of functionality
unique to Lisp such as the ability to read and write s-expressions.
