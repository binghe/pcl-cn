# Testing a File's Existence（测试文件存在）

To replace **PROBE-FILE**, you can define a function called
`file-exists-p`. It should accept a pathname and return an equivalent
pathname if the file exists and **NIL** if it doesn't. It should be able
to accept the name of a directory in either directory or file form but
should always return a directory form pathname if the file exists and
is a directory. This will allow you to use `file-exists-p`, along with
`directory-pathname-p`, to test whether an arbitrary name is the name of
a file or directory.

为了替换 **PROBE-FILE**，你可以定义一个称为 `file-exists-p`
的函数。它应当接受一个路径名，并在其代表的文件存在时返回一个等价的路径名，否则返回
**NIL**。
它应当可以接受目录形式或文件形式的目录名，但如果该文件存在并且是一个目录，那么它应当总是返回目录形式的路径名。这将允许你使用
`file-exists-p` 和
`directory-pathname-p`
来测试任意一个名字是文件名还是目录名。

In theory, `file-exists-p` is quite similar to the standard function
**PROBE-FILE**; indeed, in several implementations--SBCL, LispWorks, and
OpenMCL--**PROBE-FILE** already gives you the behavior you want for
`file-exists-p`. But not all implementations of **PROBE-FILE** behave quite
the same.

从理论上讲，`file-exists-p` 和标准函数 **PROBE-FILE**
非常相似。确实，在一些实现，即 SBCL、LispWorks 和 OpenMCL
里，**PROBE-FILE** 已经提供了 `file-exists-p`
的行为，但并非所有实现的 **PROBE-FILE** 都具有相同的行为。

Allegro and CMUCL's **PROBE-FILE** functions are close to what you
need--they will accept the name of a directory in either form but,
instead of returning a directory form name, simply return the name in
the same form as the argument it was passed. Luckily, if passed the
name of a nondirectory in directory form, they return **NIL**. So with
those implementations you can get the behavior you want by first
passing the name to **PROBE-FILE** in directory form--if the file exists
and is a directory, it will return the directory form name. If that
call returns NIL, then you try again with a file form name.

Allegro 和 CMUCL 的 **PROBE-FILE**
函数接近于你想要的行为──接受任何形式的目录名但不会返回目录形式的路径名，而只是简单地返回传给它的参数。幸运的是，如果以目录形式传递给它一个非目录的名字，它会返回
**NIL**。因此对于这些实现，为了得到想要的行为，你可以首先以目录形式将名字传给
**PROBE-FILE** 如果文件存在并且是一个目录，它将返回目录形式的名字。如果该调用返回
**NIL**，那么你可以用文件形式的名字再试一次。

CLISP, on the other hand, once again has its own way of doing
things. Its **PROBE-FILE** immediately signals an error if passed a name
in directory form, regardless of whether a file or directory exists
with that name. It also signals an error if passed a name in file form
that's actually the name of a directory. For testing whether a
directory exists, CLISP provides its own function: `probe-directory` (in
the `ext` package). This is almost the mirror image of **PROBE-FILE**: it
signals an error if passed a name in file form or if passed a name in
directory form that happens to name a file. The only difference is it
returns **T** rather than a pathname when the named directory exists.

另一方面，**CLISP** 再一次有其自己的做事方式。它的 **PROBE-FILE**
将在传递目录形式的名字时立即报错，无论该名字所代表的文件或目录是否存在。它也会在以文件形式传递一个名字且该名字实际上是一个目录的名字时报错。为了测试一个目录是否存在，CLISP
提供了它自己的函数 `probe-directory`（在 `ext`
包中）。这几乎就是 **PROBE-FILE**
的镜像：它将在传递文件形式的名字，或者目录形式的名字而刚好该名字是一个文件时报错。唯一的区别在于，当命名的目录存在时，它返回
**T** 而不是一个路径名。

But even in CLISP you can implement the desired semantics by wrapping
the calls to **PROBE-FILE** and probe-directory in **IGNORE-ERRORS**.

但就算在 CLISP 中，你也可以通过将对 **PROBE-FILE**
和 `probe-directory` 的调用包装在 **IGNORE-ERRORS**
中来实现想要的语义。

```lisp
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))
```

The function `pathname-as-file` that you need for the CLISP
implementation of `file-exists-p` is the inverse of the previously
defined `pathname-as-directory`, returning a pathname that's the file
form equivalent of its argument. This function, despite being needed
here only by CLISP, is generally useful, so define it for all
implementations and make it part of the library.

CLISP 版本的 `file-exists-p` 用到的函数 `pathname-as-file`，是前面定义的
`pathname-as-directory`
的逆函数，它返回等价于其参数的文件形式的路径名。尽管该函数只有 CLISP
用到，但它通常很有用，因此我们为所有实现定义它并使其成为该库的一部分。

```lisp
(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))
```

