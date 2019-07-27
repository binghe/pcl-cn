# Constructing New Pathnames（构造新路径名）

You can construct arbitrary pathnames using the **MAKE-PATHNAME**
function. It takes one keyword argument for each pathname component
and returns a pathname with any supplied components filled in and the
rest **NIL**.

你可以使用 **MAKE-PATHNAME**
函数来构造任意路径名。它对每个路径名组件都接受一个关键字参数并返回一个路径名，任何提供了的组件都被填入其中而其余的为 **NIL**。

```lisp
(make-pathname
  :directory '(:absolute "foo" "bar")
  :name "baz"
  :type "txt") ==> #p"/foo/bar/baz.txt"
```

However, if you want your programs to be portable, you probably don't
want to make pathnames completely from scratch: even though the
pathname abstraction protects you from unportable filename syntax,
filenames can be unportable in other ways. For instance, the filename
`/home/peter/foo.txt` is no good on an OS X box where `/home/` is called
`/Users/`.

不过，如果你希望程序是可移植的，你不会想要完全用手工生成路径名：就算路径名抽象可以保护你免于使用不可移植的文件名语法，文件名也可能以其他方式不可移植。例如，文件名
`/home/peter/foo.txt` 对于 Mac OS X
来说就不是一个好的文件名，因为在那里 `/home/` 被称为 `/Users/`。

Another reason not to make pathnames completely from scratch is that
different implementations use the pathname components slightly
differently. For instance, as mentioned previously, some Windows-based
Lisp implementations store the drive letter in the device component
while others store it in the host component. If you write code like
this:

不推荐完全用手工生成路径名的另一个原因是，不同的实现使用路径名组件的方式略有差异。例如，前面已经提到过，某些基于 Windows 的 Lisp
实现会将驱动器字母保存在设备组件中，而其他一些实现则将它保存在主机组件中。如果你将代码写成这样：

```lisp
(make-pathname :device "c" :directory '(:absolute "foo" "bar") :name "baz")
```

it will be correct on some implementations but not on others.

那么它在一些实现里将是正确的而在其他实现里则不是。

Rather than making names from scratch, you can build a new pathname
based on an existing pathname with **MAKE-PATHNAME**'s keyword parameter
`:defaults`. With this parameter you can provide a pathname designator,
which will supply the values for any components not specified by other
arguments. For example, the following expression creates a pathname
with an `.html` extension and all other components the same as the
pathname in the variable `input-file`:

与其用手工生成路径名，你还可以基于一个已有的路径名使用
**MAKE-PATHNAME** 的关键字参数 `:defaults`
来构造一个新路径名。你可以为该参数提供一个路径名描述符，它将提供没有被其他参数指定的任何组件的值。例如，下面的表达式创建了一个带有 `.html`
扩展名的路径名，同时所有其他组件都与变量 `input-file` 中的路径名相同：

```lisp
(make-pathname :type "html" :defaults input-file)
```

Assuming the value in `input-file` was a user-provided name, this code
will be robust in the face of operating system and implementation
differences such as whether filenames have drive letters in them and
where they're stored in a pathname if they do.

假设 `input-file`
中的值是一个用户提供的名字，这一代码对于操作系统和实现的区别来说是健壮的，无论文件名是否带有驱动器字母或是它被保存在路径名的哪个组件上。

You can use the same technique to create a pathname with a different
directory component.

你可以使用相同的技术来创建一个带有不同目录组件的路径名。

```lisp
(make-pathname :directory '(:relative "backups") :defaults input-file)
```

However, this will create a pathname whose whole directory component
is the relative directory `backups/`, regardless of any directory
component `input-file` may have had. For example:

不过，这会创建出一种整个目录组件是相对目录 `backups/`
的路径名，而不管 `input-file` 是否可能会有任何目录组件。例如：

```lisp
(make-pathname :directory '(:relative "backups")
               :defaults #p"/foo/bar/baz.txt") ==> #p"backups/baz.txt"
```

Sometimes, though, you want to combine two pathnames, at least one of
which has a relative directory component, by combining their directory
components. For instance, suppose you have a relative pathname such as
`#p"foo/bar.html"` that you want to combine with an absolute pathname
such as `#p"/www/html/"` to get `#p"/www/html/foo/bar.html"`. In that
case, **MAKE-PATHNAME** won't do; instead, you want **MERGE-PATHNAMES**.

但有时你会想要通过合并两个路径名的目录组件合在一起来组合两个路径名，其中至少一个带有相对的目录组件。例如，假设有一个诸如
`#p"foo/bar.html"`
的相对路径名，你想将它与一个诸如 `#p"/www/html/"`
这样的绝对路径名组合起来得到
`#p"/www/html/foo/bar.html"`。在这种情况下，**MAKE-PATHNAME**
将无法处理。相反，你需要的是 **MERGE-PATHNAMES**。

**MERGE-PATHNAMES** takes two pathnames and merges them, filling in any
**NIL** components in the first pathname with the corresponding value from
the second pathname, much like **MAKE-PATHNAME** fills in any unspecified
components with components from the `:defaults` argument. However,
**MERGE-PATHNAMES** treats the directory component specially: if the first
pathname's directory is relative, the directory component of the
resulting pathname will be the first pathname's directory relative to
the second pathname's directory. Thus:

**MERGE-PATHNAMES**
接受两个路径名并合并它们，用来自第二个路径名的对应值填充第一个路径名中的任何
**NIL** 组件，这和 **MAKE-PATHNAME** 使用来自 `:defaults`
参数的组件来填充任何未指定的组件非常相似。不过，**MERGE-PATHNAMES**
会特别对待目录组件：如果第一个组件名的目录是相对的，那么生成的路径名的目录组件将是第一个路径名的目录相对于第二个路径名的目录。这样：

```lisp
(merge-pathnames #p"foo/bar.html" #p"/www/html/") ==> #p"/www/html/foo/bar.html"
```

The second pathname can also be relative, in which case the resulting
pathname will also be relative.

第二个路径名也可以是相对的，在这种情况下得到的路径名也将是相对的：

```lisp
(merge-pathnames #p"foo/bar.html" #p"html/") ==> #p"html/foo/bar.html"
```

To reverse this process and obtain a filename relative to a particular
root directory, you can use the handy function **ENOUGH-NAMESTRING**.

为了反转这一过程以便获得一个相对于特定根目录的文件名，你可以使用一个便利的函数
**ENOUGH-NAMESTRING**：

```lisp
(enough-namestring #p"/www/html/foo/bar.html" #p"/www/") ==> "html/foo/bar.html"
```

You can then combine **ENOUGH-NAMESTRING** with **MERGE-PATHNAMES** to create
a pathname representing the same name but in a different root.

你还可以组合 **ENOUGH-NAMESTRING** 和 **MERGE-PATHNAMES**
来创建一个表达相同名字但却在不同根目录中的路径名。

```lisp
(merge-pathnames
  (enough-namestring #p"/www/html/foo/bar/baz.html" #p"/www/")
  #p"/www-backups/") ==> #p"/www-backups/html/foo/bar/baz.html"
```

**MERGE-PATHNAMES** is also used internally by the standard functions that
actually access files in the file system to fill in incomplete
pathnames. For instance, suppose you make a pathname with just a name
and a type.

**MERGE-PATHNAMES**
也被用来实际访问文件系统中标准函数内部用于填充不完全的路径名的文件。例如，假设有一个只有名称和类型的路径名：

```lisp
(make-pathname :name "foo" :type "txt") ==> #p"foo.txt"
```

If you try to use this pathname as an argument to **OPEN**, the missing
components, such as the directory, must be filled in before Lisp will
be able to translate the pathname to an actual filename. Common Lisp
will obtain values for the missing components by merging the given
pathname with the value of the variable
`*DEFAULT-PATHNAME-DEFAULTS*`. The initial value of this variable is
determined by the implementation but is usually a pathname with a
directory component representing the directory where Lisp was started
and appropriate values for the host and device components, if
needed. If invoked with just one argument, **MERGE-PATHNAMES** will merge
the argument with the value of `*DEFAULT-PATHNAME-DEFAULTS*`. For
instance, if `*DEFAULT-PATHNAME-DEFAULTS*` is `#p"/home/peter/"`, then
you'd get the following:

如果想用这个路径名作为 **OPEN**
的一个参数，那么缺失的组件（诸如目录）就必须在 Lisp
可以将路径名转化成一个实际文件名之前被填充进去。Common Lisp
将通过合并给定路径名与变量 `*DEFAULT-PATHNAME-DEFAULTS*`
中的值来获得缺失组件的值。该变量的初始值由具体实现所决定，但通常是一个路径名，其目录组件表示
Lisp
启动时所在的目录，主机和设备组件如果需要的话也带有适当的值。如果只有一
个参数被调用的话，**MERGE-PATHNAMES**
将把该参数与 `*DEFAULT-PATHNAME-DEFAULTS*` 的值进行合并。例如，如果
`*DEFAULT-PATHNAME-DEFAULTS*` 是 `#p"/home/peter/"`，那么你将得到下面的结果：

```lisp
(merge-pathnames #p"foo.txt") ==> #p"/home/peter/foo.txt"
```

