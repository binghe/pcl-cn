# Packaging the Library（给库打包）

Speaking of packages, if you download the complete code for this
library, you'll see that it's defined in a new package,
`com.gigamonkeys.pathnames`. I'll discuss the details of defining and
using packages in Chapter 21. For now you should note that some
implementations provide their own packages that contain functions with
some of the same names as the ones you'll define in this chapter and
make those names available in the `CL-USER` package. Thus, if you try to
define the functions from this library while in the `CL-USER` package,
you may get errors or warnings about clobbering existing
definitions. To avoid this possibility, you can create a file called
`packages.lisp` with the following contents:

从包的角度讲，如果你下载了该库的完整代码，会看到它被定义在一个新的包
`com.gigamonkeys.pathnames` 中。我将在第 21
章讨论定义使用包的细节。目前你应当注意，某些实现提供了它们自己的包，其中含有一些函数与你将在本章中定义的一些函数有相同的名字，并且这些名字可在
`CL-USER` 包中访问。这样，如果你试图在 `CL-USER`
包中定义该库中的某些函数，可能会得到关于破坏了已有定义的错误或警告。为了避免发生这种情况，你可以创建一个称为
`packages.lisp` 的文件，其中带有下面的内容：

```lisp
(in-package :cl-user)

(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))
```

and **LOAD** it. Then at the REPL or at the top of the file where you type
the definitions from this chapter, type the following expression:

并加载它，然后在 REPL 中或者在你输入定义的文件顶端，输入下列表达式：

```lisp
(in-package :com.gigamonkeys.pathnames)
```

In addition to avoiding name conflicts with symbols already available
in `CL-USER`, packaging the library this way also makes it easier to use
in other code, as you'll see in several future chapters.

将库以这种方形式打包，除了可以避免与那些已存在于 `CL-USER`
包中的符号产生冲突以外，还可以使其更容易被其他代码使用，你在后续几章中将看到这一点。
