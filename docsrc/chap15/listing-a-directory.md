# Listing a Directory（列目录）

You can implement the function for listing a single directory,
`list-directory`, as a thin wrapper around the standard function
**DIRECTORY**. **DIRECTORY** takes a special kind of pathname, called a wild
pathname, that has one or more components containing the special value
`:wild` and returns a list of pathnames representing files in the file
system that match the wild pathname. The matching algorithm--like
most things having to do with the interaction between Lisp and a
particular file system--isn't defined by the language standard, but
most implementations on Unix and Windows follow the same basic scheme.

你可以将用于列举单独目录的函数 `list-directory`，实现成标准函数
**DIRECTORY**
外围的一个包装层。**DIRECTORY**
接受一种特殊类型的路径名，称为通配路径名，其带有一个或多个含有特殊值
`:wild`
的组件，然后返回一个路径名的列表，用来表示文件系统中匹配该通配路径名的文件。 匹配算法和多数
Lisp 与一个特定文件系统之间的交互一致，它没有被语言标准定义，但
Unix 与 Windows 上的多数实现遵循了相同的基本模式。

The **DIRECTORY** function has two problems that you need to address with
`list-directory`. The main one is that certain aspects of its behavior
differ fairly significantly between different Common Lisp
implementations, even on the same operating system. The other is that
while **DIRECTORY** provides a powerful interface for listing files, to
use it properly requires understanding some rather subtle points about
the pathname abstraction. Between these subtleties and the
idiosyncrasies of different implementations, actually writing portable
code that uses **DIRECTORY** to do something as simple as listing all the
files and subdirectories in a single directory can be a frustrating
experience. You can deal with those subtleties and idiosyncrasies once
and for all, by writing `list-directory`, and forget them thereafter.

**DIRECTORY** 函数有两个需要在 `list-directory`
中解决的问题。其中主要的一个是，即便在相同的操作系统上，其行为的特定方面在不同的
Common Lisp 的实现间也具有相当大的区别。另一个问题在于，尽管
**DIRECTORY**
提供了一个强大的用于列举文件的接口，但要想正确地使用它需要对路径名抽象有相当细致的理解。在这些不同细微之处和不同实现的特征影响下，实际编写可移植代码来使用
**DIRECTORY**
一些像列出单个目录中所有文件和子目录这样简单的事情，都可能令人沮丧。你可以通过编写
`list-directory`
来一次性地处理所有这些细节和特征，并从此忘记它们。

One subtlety I discussed in Chapter 14 is the two ways to represent
the name of a directory as a pathname: directory form and file form.

我在第 14
章里讨论过一个细节是，将一个目录的名字表示成路径名有两种方式，即目录形式和文件形式。

To get **DIRECTORY** to return a list of files in `/home/peter/`, you need
to pass it a wild pathname whose directory component is the directory
you want to list and whose name and type components are `:wild`. Thus,
to get a listing of the files in `/home/peter/`, it might seem you could
write this:

为了让 **DIRECTORY** 返回 `/home/peter/`
中的文件列表，需要传给它一个通配路径名，其目录组件是你想要列出的目录，其名称和类型组件是
`:wild`。因此，为了列出 `/home/peter/` 中的文件，需要这样来写：

```lisp
(directory (make-pathname :name :wild :type :wild :defaults home-dir))
```

where `home-dir` is a pathname representing `/home/peter/`. This would
work if `home-dir` were in directory form. But if it were in file
form--for example, if it had been created by parsing the namestring
"/home/peter"--then that same expression would list all the files in
`/home` since the name component "peter" would be replaced with `:wild`.

其中的 `home-dir` 是代表 `/home/peter/` 的路径名。如果
`home-dir`
是以目录形式表示的，那么上述写法将是有效的。但如果它以文件形式表示，例如，它通过解析名字字符串
"/home/peter" 来创建，那么同样的表达式将列出 `/home`
中的所有文件，因为名字组件 "peter" 将被替换成 `:wild`。

To avoid having to worry about explicitly converting between
representations, you can define `list-directory` to accept a nonwild
pathname in either form, which it will then convert to the appropriate
wild pathname.

为了避免两种形式间的显式转换，可以定义 `list-directory`
来接受任何形式的非通配路径名，随后它将被转化成适当的通配路径名。

To help with this, you should define a few helper functions. One,
`component-present-p`, will test whether a given component of a pathname
is "present," meaning neither NIL nor the special value
`:unspecific`. Another, directory-pathname-p, tests whether a pathname
is already in directory form, and the third, pathname-as-directory,
converts any pathname to a directory form pathname.

为此，应当定义一些助手函数。其中一个是
`component-present-p`，它将测试一个路径名的给定组件是否
“存在”，也就是说该组件既不是 **NIL** 也不是特殊值 `:unspecific`。 另一个函数是
`directory-pathname-p`，用来测试一个路径名是否已经是目录形式，而第三个函数
`pathname-as-directory` 可以将任何路径名转换成目录形式的路径名。

```lisp
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))
```

Now it seems you could generate a wild pathname to pass to **DIRECTORY**
by calling **MAKE-PATHNAME** with a directory form name returned by
`pathname-as-directory`. Unfortunately, it's not quite that simple,
thanks to a quirk in CLISP's implementation of **DIRECTORY**. In CLISP,
**DIRECTORY** won't return files with no extension unless the type
component of the wildcard is **NIL** rather than `:wild`. So you can define
a function, `directory-wildcard`, that takes a pathname in either
directory or file form and returns a proper wildcard for the given
implementation using read-time conditionalization to make a pathname
with a `:wild` type component in all implementations except for CLISP
and **NIL** in CLISP.

现在看起来似乎你可以通过在由 `pathname-as-directory`
返回的目录形式名字上调用 **MAKE-PATHNAME**，来生成一个通配路径名并传给
**DIRECTORY**。不幸的是，事情没有那么简单，多亏了 CLISP 的
**DIRECTORY** 实现中的一个怪癖。在 CLISP
中，除非通配符中的类型组件是 **NIL** 而非 `:wild`，**DIRECTORY**
将不会返回那些没有扩展名的文件。因此，你可以定义一个函数
`directory-wildcard`，其接受一个目录形式或文件形式的路径名，并返回一个给定实现下的适当的通配符，它通过使用读取期条件化在除
CLISP 之外的所有实现里生成一个带有 `:wild`
类型组件的路径名，而在 CLISP 中该类型组件为 **NIL**。

```lisp
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))
```

Note how each read-time conditional operates at the level of a single
expression After `#-clisp`, the expression `:wild` is either read or
skipped; likewise, after `#+clisp`, the **NIL** is read or skipped.

注意每一个读取期条件是怎样在单个表达式层面上操作的。在
`#-clisp` 之后，表达式 `:wild`
要么被读取要么被跳过；同样，在 `#+clisp`
之后，**NIL** 要么被读取要么被跳过。

Now you can take a first crack at the `list-directory` function.

现在你可以首次看到 `list-directory` 函数了：

```lisp
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))
```

As it stands, this function would work in SBCL, CMUCL, and
LispWorks. Unfortunately, a couple more implementation differences
remain to be smoothed over. One is that not all implementations will
return subdirectories of the given directory. Allegro, SBCL, CMUCL,
and LispWorks do. OpenMCL doesn't by default but will if you pass
**DIRECTORY** a true value via the implementation-specific keyword
argument `:directories`. CLISP's **DIRECTORY** returns subdirectories only
when it's passed a wildcard pathname with :wild as the last element of
the directory component and NIL name and type components. In this
case, it returns only subdirectories, so you'll need to call DIRECTORY
twice with different wildcards and combine the results.

使用上述定义，该函数在 SBCL、CMUCL 和 LispWorks
中正常工作。不幸的是，你还需要谨慎处理另外一些实现间的区别。其中一点是，并非所有实现都返回给定目录中的子目录。Allegro、SBCL、CMUCL
和 LispWorks 可以返回子目录。OpenMCL
默认不会这样做，但如果为 **DIRECTORY**
传递一个实现相关的、值为真的关键字参数
`:directories`，那么它将返回子目录。对于 CLISP 的
**DIRECTORY** 只有当传递给它一个以 `:wild`
作为目录组件的最后一个元素且名字和类型组件为 **NIL**
的通配路径名时，才可以返回子目录。而且，在这种情况下，它只返回子目录，因此需要使用不同的通配符，调用
**DIRECTORY** 两次并组合结果。

Once you get all the implementations returning directories, you'll
discover they can also differ in whether they return the names of
directories in directory or file form. You want list-directory to
always return directory names in directory form so you can
differentiate subdirectories from regular files based on just the
name. Except for Allegro, all the implementations this library will
support do that. Allegro, on the other hand, requires you to pass
**DIRECTORY** the implementation-specific keyword argument
`:directories-are-files` **NIL** to get it to return directories in file
form.

一旦所有实现都返回目录了，你会发现它们返回的目录名有些是目录形式的，有
些是文件形式的。你想要
`list-directory`
总是返回目录形式的目录名，以便可以只通过名字来区分子目录和正规文件。除
Allegro 之外，所有实现都支持做到这点。Allegro
要求为 **DIRECTORY** 传递一个实现相关的、值为 NIL
的关键字参数 `:directories-are-files`，从而使其以目录形式返回目录。

Once you know how to make each implementation do what you want,
actually writing list-directory is simply a matter of combining the
different versions using read-time conditionals.

一旦你知道如何使每一个实现做到你想要的事，那么实际编写
`list-directory`
就只是简单地将不同版本用读取期条件组合起来了。

```lisp
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))
```

The function `clisp-subdirectories-wildcard` isn't actually specific to
CLISP, but since it isn't needed by any other implementation, you can
guard its definition with a read-time conditional. In this case, since
the expression following the `#+` is the whole **DEFUN**, the whole function
definition will be included or not, depending on whether `clisp` is
present in `*FEATURES*`.

函数 `clisp-subdirectories-wildcard` 事实上并是非 CLISP
相关的，由于任何其他实现不需要它，因而可以将其定义放在一个读取期条件之后。在这种情况下，由于跟在
`#+` 后面的表达式是整个 **DEFUN**，因此整个函数定义是否被包含将取决于
`clisp` 是否存在于 *FEATURES* 中。

```lisp
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard)) 
```
