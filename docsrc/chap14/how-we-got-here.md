# How We Got Here（进化历程）

The historical diversity of file systems in existence during the 70s
and 80s can be easy to forget. Kent Pitman, one of the principal
technical editors of the Common Lisp standard, described the situation
once in `comp.lang.lisp` (Message-ID: `sfwzo74np6w.fsf@world.std.com`)
thusly:

存在于上世纪 70 和 80
年代的文件系统的历史多样性很容易被遗忘。Kent Pitman，Common Lisp
标准的主要技术编辑之一，有一次在 `comp.lang.lisp` （Message-ID: `sfwzo74np6w.fsf@world.std.com`）新闻组上描述了如下情形：

> The dominant file systems at the time the design [of Common Lisp]
> was done were TOPS-10, TENEX, TOPS-20, VAX VMS, AT&T Unix, MIT
> Multics, MIT ITS, not to mention a bunch of mainframe [OSs]. Some
> were uppercase only, some mixed, some were case-sensitive but case-
> translating (like CL). Some had dirs as files, some not. Some had
> quote chars for funny file chars, some not. Some had wildcards, some
> didn't. Some had `:up` in relative pathnames, some didn't. Some had
> namable root dirs, some didn't. There were file systems with no
> directories, file systems with non-hierarchical directories, file
> systems with no file types, file systems with no versions, file
> systems with no devices, and so on.

> 在 Common Lisp 的设计完成时期，处于支配地位的文件系统是：TOPS-10、TENEX、TOPS-20、VAX VMS、AT&T Unix、MIT Multics、MIT
> ITS，更不用说还有许多大型机操作系统了。它们中的一些只支持大写字母，一些是大小写混合的，另一些则是大小写敏感但却能自动作大小写转换（就像
> Common Lisp）。它们中的一些将目录视为文件，而一些则不会。一些对于特殊的文件字符带有引用字符，另一些不会。一些带有通配符，而另一些没有。一些在相对路径名中使用
> `:up`，另一些不这样做。一些带有可命名的根目录，而另一些没有。还存在没有目录的文件系统，使用非层次目录结构的文件系统，不支持文件类型的文件系统，没有版本的文件系统以及没有设备的文件系统，等等。

If you look at the pathname abstraction from the point of view of any
single file system, it seems baroque. However, if you take even two
such similar file systems as Windows and Unix, you can already begin
to see differences the pathname system can help abstract away--Windows
filenames contain a drive letter, for instance, while Unix filenames
don't. The other advantage of having the pathname abstraction designed
to handle the wide variety of file systems that existed in the past is
that it's more likely to be able to handle file systems that may exist
in the future. If, say, versioning file systems come back into vogue,
Common Lisp will be ready.

如果从任何单一文件系统的观点上观察路径名抽象，那么它看起来显得过于复杂。不过，如果考察两种像
Windows 和 Unix 这样相似的文件系统，你可能已经开始注意路径名系统可能帮你抽象掉的一些区别了。例如，Windows
文件名含有一个驱动器字母，而 Unix
文件名却没有。使用这种用来处理过去存在的广泛的文件系统的路径名抽象带来的另一种好处是，它有可能可以处理将来可能存在的文件系统。比如说，如果版本文件系统重新流行起来的话，Common Lisp 就已经准备好了。

