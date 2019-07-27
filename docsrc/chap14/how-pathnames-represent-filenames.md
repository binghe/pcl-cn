# How Pathnames Represent Filenames（路径名如何表示文件名）

A pathname is a structured object that represents a filename using six
components: host, device, directory, name, type, and version. Most of
these components take on atomic values, usually strings; only the
directory component is further structured, containing a list of
directory names (as strings) prefaced with the keyword `:absolute` or
`:relative`. However, not all pathname components are needed on all
platforms--this is one of the reasons pathnames strike many new
Lispers as gratuitously complex. On the other hand, you don't really
need to worry about which components may or may not be used to
represent names on a particular file system unless you need to create
a new pathname object from scratch, which you'll almost never need to
do. Instead, you'll usually get hold of pathname objects either by
letting the implementation parse a file system-specific namestring
into a pathname object or by creating a new pathname that takes most
of its components from an existing pathname.

路径名是一种使用6个组件来表示文件名的结构化对象：主机（host）、设备（device）、目录（directory）、名称（name）、类型（type）以及版本（version）。这些组件的多数都接受原子值，通常是字符串。只有目录组件有其进一步的结构，含有一个目录名（作为字符串）的列表，其中带有关键字
`:absolute` 或 `:relative`
作为前缀。但并非所有路径名组件在所有平台上都是必须的──这也是路径名让许多初级
Lisp
程序员感到无端复杂的原因之一。另一方面，你真的不必担心哪个组件在特定文件系统上是否可被用来表示文件名，除非你需要手工地从头创建一个新路径名对象。相反，通常将通过让具体实现来把一个文件系统相关的名字字符串解析到一个路径名对象，或是通过从一个已有路径名中取得其多数组件来创建新路径名，从而得到路径名对象。

For instance, to translate a namestring to a pathname, you use the
**PATHNAME** function. It takes a pathname designator and returns an
equivalent pathname object. When the designator is already a pathname,
it's simply returned. When it's a stream, the original filename is
extracted and returned. When the designator is a namestring, however,
it's parsed according to the local filename syntax. The language
standard, as a platform-neutral document, doesn't specify any
particular mapping from namestring to pathname, but most
implementations follow the same conventions on a given operating
system.

例如，为了将名字字符串转化成路径名，你可以使用
**PATHNAME**
函数。它接受路径名描述符并返回等价的路径名对象。当该描述符已经是一个路径名时，它就会被简单地返回。当它是一个流时，最初的文件名就会被抽取出然后返回。不过当描述符是一个名字字符串时，它将根据本地文件名语法来被解析。作为一个平台中立的文档，语言标准没有指定任何从名字字符串到路径名的特定映射，但是多数实现遵守了与其所在操作系统相同的约定。

On Unix file systems, only the directory, name, and type components
are typically used. On Windows, one more component--usually the device
or host--holds the drive letter. On these platforms, a namestring is
parsed by first splitting it into elements on the path separator--a
slash on Unix and a slash or backslash on Windows. The drive letter on
Windows will be placed into either the device or the host
component. All but the last of the other name elements are placed in a
list starting with `:absolute` or `:relative` depending on whether the
name (ignoring the drive letter, if any) began with a path
separator. This list becomes the directory component of the
pathname. The last element is then split on the rightmost dot, if any,
and the two parts put into the name and type components of the
pathname.

在 Unix 文件系统上，只有目录、名称和类型组件通常会被用到。在 Windows
上，还有一个组件（通常是设备或主机）保存了驱动器字母。在这些平台上，一个名字字符串在解析时首先被路径分隔符──会在
Unix 上是一个斜杠而在 Windows
上是一个斜杠或反斜杠──分拆成基本元素。在 Windows
上，驱动器字母要么被放置在设备中，要么就是在主机组件中。其他名字元素除最后一个之外都被放置在一个以
`:absolute` 或 `:relative`
开始的列表中，具体取决于该名字是否（如果有的话，忽略驱动器字母）以一个路径分隔符开始。这个列表将成为路径名的目录组件。如果有的话，最后一个元素将在最右边的点处被分拆开，然后得到的两部分将被放进路径名的名称和类型组件中。

You can examine these individual components of a pathname with the
functions **PATHNAME-DIRECTORY**, **PATHNAME-NAME**, and **PATHNAME-TYPE**.

你可以使用函数 **PATHNAME-DIRECTORY**、**PATHNAME-NAME**
和 **PATHNAME-TYPE** 来检查一个路径名中的单独组件。

```lisp
(pathname-directory (pathname "/foo/bar/baz.txt")) ==> (:ABSOLUTE "foo" "bar")
(pathname-name (pathname "/foo/bar/baz.txt"))      ==> "baz"
(pathname-type (pathname "/foo/bar/baz.txt"))      ==> "txt"
```

Three other functions--**PATHNAME-HOST**, **PATHNAME-DEVICE**, and
**PATHNAME-VERSION**--allow you to get at the other three pathname
components, though they're unlikely to have interesting values on
Unix. On Windows either **PATHNAME-HOST** or **PATHNAME-DEVICE** will return
the drive letter.

其他三个函数 **PATHNAME-HOST**、**PATHNAME-DEVICE** 和
**PATHNAME-VERSION**
允许你访问其他三个路径名组件，尽管它们在 Unix 上不太可能带有感兴趣的值。在
Windows 上，**PATHNAME-HOST** 和 **PATHNAME-DEVICE**
两者之一将返回驱动器字母。

Like many other built-in objects, pathnames have their own read
syntax, `#p` followed by a double-quoted string. This allows you to
print and read back s-expressions containing pathname objects, but
because the syntax depends on the namestring parsing algorithm, such
data isn't necessarily portable between operating systems.

和许多其他内置对象一样，路径名也有其自身的读取语法：`#p`
后接一个双引号字符串。这允许你打印并且读回含有路径名对象的
S-表达式，但由于其语法取决于名字字符串解析算法，这些数据在操作系统之间不一定可移植。

```lisp
(pathname "/foo/bar/baz.txt") ==> #p"/foo/bar/baz.txt"
```

To translate a pathname back to a namestring--for instance, to present
to the user--you can use the function **NAMESTRING**, which takes a
pathname designator and returns a namestring. Two other functions,
**DIRECTORY-NAMESTRING** and **FILE-NAMESTRING**, return a partial
namestring. **DIRECTORY-NAMESTRING** combines the elements of the
directory component into a local directory name, and **FILE-NAMESTRING**
combines the name and type components.

为了将一个路径名转化回一个名字字符串，例如，为了呈现给用户，你可以使用函数
**NAMESTRING**，其接受一个路径名描述符并返回一个名字字符串。其他两个函数
**DIRECTORY-NAMESTRING** 和 **FILE-NAMESTRING**
返回一个部分名字字符串。**DIRECTORY-NAMESTRING**
将目录组件的元素组合成一个本地目录名，而 **FILE-NAMESTRING**
则组合名字和类型组件。

```lisp
(namestring #p"/foo/bar/baz.txt")           ==> "/foo/bar/baz.txt"
(directory-namestring #p"/foo/bar/baz.txt") ==> "/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")      ==> "baz.txt"
```
