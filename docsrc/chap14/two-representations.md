# Two Representations of Directory Names（目录名的两种表示方法）

When dealing with pathnames that name directories, you need to be
aware of one wrinkle. Pathnames separate the directory and name
components, but Unix and Windows consider directories just another
kind of file. Thus, on those systems, every directory has two
different pathname representations.

当处理命名目录的路径名时，你需要注意一点。路径名将目录和名称组件区分开，但
Unix 和 Windows
却将目录视为另一种类型的文件。这样，在这些系统里，每一个目录都有两种不同的路径名表示方法。

One representation, which I'll call file form, treats a directory like
any other file and puts the last element of the namestring into the
name and type components. The other representation, directory form,
places all the elements of the name in the directory component,
leaving the name and type components **NIL**. If `/foo/bar/` is a directory,
then both of the following pathnames name it.

一种表示方法，我将它称为文件形式（file
form），将目录当成像其他任何文件一样来对待，将名字字符串中的最后一个元素放在名称和类型组件中。另一种表示方法，目录形式（directory
form）将名字中的所有元素都放在目录组件中，而留下名称和类型组件为
**NIL**。如果 `/foo/bar/` 是一个目录，那么下面两个路径名都可以命名它：

```lisp
(make-pathname :directory '(:absolute "foo") :name "bar") ; file form
(make-pathname :directory '(:absolute "foo" "bar"))       ; directory form
```

When you create pathnames with **MAKE-PATHNAME**, you can control which
form you get, but you need to be careful when dealing with
namestrings. All current implementations create file form pathnames
unless the namestring ends with a path separator. But you can't rely
on user-supplied namestrings necessarily being in one form or
another. For instance, suppose you've prompted the user for a
directory to save a file in and they entered "/home/peter". If you
pass that value as the `:defaults` argument of **MAKE-PATHNAME** like this:

当你用 **MAKE-PATHNAME**
创建路径名时，你可以控制得到的形式，但你需要在处理名字字符串时多加小心。当前所有实现都创建文件形式的路径名，除非名字字符串以一个路径分隔符结尾。但你不能依赖于用户提供的名字字符串必须是以一种或另一种形式。例如，当你提示用户输入一个用来保存文件的目录而它们输入了
"/home/peter"。如果你像下面这样将该值作为 **MAKE-PATHNAME** 的
`:defaults` 参数：

```lisp
(make-pathname :name "foo" :type "txt" :defaults user-supplied-name)
```

you'll end up saving the file in `/home/foo.txt` rather than the
intended `/home/peter/foo.txt` because the "peter" in the namestring
will be placed in the name component when `user-supplied-name` is
converted to a pathname. In the pathname portability library I'll
discuss in the next chapter, you'll write a function called
`pathname-as-directory` that converts a pathname to directory form. With
that function you can reliably save the file in the directory
indicated by the user.

那么最后你将把文件保存成 `/home/foo.txt`
而不是你想要的 `/home/peter/foo.txt`，因为当 `user-supplied-name`
被转化成一个路径名时，名字字符串中的 "peter"
将被放在名称组件中。在下一章我将讨论的路径名可移植库中，你将编写一个名为
`pathname-as-directory`
的函数，它将一个路径名转化成目录形式。使用该函数，你可以放心地在用户给出的目录里保存文件。

```lisp
(make-pathname
  :name "foo" :type "txt" :defaults (pathname-as-directory user-supplied-name)) 
```
