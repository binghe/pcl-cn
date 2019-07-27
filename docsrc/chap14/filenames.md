# Filenames（文件名）

So far you've used strings to represent filenames. However, using
strings as filenames ties your code to a particular operating system
and file system. Likewise, if you programmatically construct names
according to the rules of a particular naming scheme (separating
directories with `/`, say), you also tie your code to a particular file
system.

到目前为止，文件名都是用字符串表示的。但使用字符串作为文件名会将代码捆绑在特定操作系统和文件系统上。而且如果按照一个特定的文件命名方案的规则（比如说，使用
`/` 来分隔目录）用程序来构造文件名，那么你就会将代码捆绑到特定的文件系统上。

To avoid this kind of nonportability, Common Lisp provides another
representation of filenames: pathname objects. Pathnames represent
filenames in a structured way that makes them easy to manipulate
without tying them to a particular filename syntax. And the burden of
translating back and forth between strings in the local syntax--called
namestrings--and pathnames is placed on the Lisp implementation.

为了避免这种不可移植性，Common Lisp
提供了另一种文件名的表示方式：路径名（pathname）对象。路径名以一种结构化的方式来表示文件名，这种方式使得
它们易于管理而无须捆绑在特定的文件名语法上。而在以本地语法写成的字符串，即名字字符串（namestring），和路径名之间进行来回转换的责任则被放在了
Lisp 实现身上。

Unfortunately, as with many abstractions designed to hide the details
of fundamentally different underlying systems, the pathname
abstraction introduces its own complications. When pathnames were
designed, the set of file systems in general use was quite a bit more
variegated than those in common use today. Consequently, some nooks
and crannies of the pathname abstraction make little sense if all
you're concerned about is representing Unix or Windows
filenames. However, once you understand which parts of the pathname
abstraction you can ignore as artifacts of pathnames' evolutionary
history, they do provide a convenient way to manipulate filenames.

不幸的是，如同许多被设计用于隐藏本质上不同的底层系统细节的抽象那样，路径名抽象也引入了它们自己的复杂性。当路径名最初被设计时，通常使用的文件系统集合比今天所使用的更加丰富多彩。这带来的结果是，在你只关心如何表示
Unix 或 Windows 文件名时，路径名抽象的某些细微之处就没有什么意义了。不过，一旦你理解了路径名抽象中的哪些部分可以作为路径名发展史中的遗留产物而忽略时，你就会发现它们确实提供了一种管理文件名的便捷方式。

Most places a filename is called for, you can use either a namestring
or a pathname. Which to use depends mostly on where the name
originated. Filenames provided by the user--for example, as arguments
or as values in configuration files--will typically be namestrings,
since the user knows what operating system they're running on and
shouldn't be expected to care about the details of how Lisp represents
filenames. But programmatically generated filenames will be pathnames
because you can create them portably. A stream returned by OPEN also
represents a filename, namely, the filename that was originally used
to open the stream. Together these three types are collectively
referred to as pathname designators. All the built-in functions that
expect a filename argument accept all three types of pathname
designator. For instance, all the places in the previous section where
you used a string to represent a filename, you could also have passed
a pathname object or a stream.

在多数使用文件名的调用场合里，你都可以同时使用个名字字符串或是路径名。具体使用哪一个在很大程度上取决于该名字的来源。由用户提供的文件名（例如作为参数或是配置文件中的值）通常是名字字符串，因为用户只知道它们所运行的文件系统而不关心
Lisp 表示文件名的细节。但通过编程方法产生的文件名是路径名，因为你能可移植地创建它们。一个由
**OPEN** 返回的流也代表文件名，也就是那个当初用来打开该流的文件名。这三种类型的文件名被总称为路径名描述符（pathname
designator）。所有内置的以文件名作为参数的函数都能接受所有这三种路径名描述符。例如，前面章节里所有的用字符串来表示文件名的位置都同样可以传入路径名对象或流。

