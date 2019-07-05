# Choosing a Lisp Implementation（选择一个 Lisp 实现）

The first thing you have to do is to choose a Lisp
implementation. This may seem like a strange thing to have to do for
folks used to languages such as Perl, Python, Visual Basic (VB), C#,
and Java. The difference between Common Lisp and these languages is
that Common Lisp is defined by its standard--there is neither a single
implementation controlled by a benevolent dictator, as with Perl and
Python, nor a canonical implementation controlled by a single company,
as with VB, C#, and Java. Anyone who wants to read the standard and
implement the language is free to do so. Furthermore, changes to the
standard have to be made in accordance with a process controlled by
the standards body American National Standards Institute (ANSI). That
process is designed to keep any one entity, such as a single vendor,
from being able to arbitrarily change the standard. Thus, the Common
Lisp standard is a contract between any Common Lisp vendor and Common
Lisp programmers. The contract tells you that if you write a program
that uses the features of the language the way they're described in
the standard, you can count on your program behaving the same in any
conforming implementation.

首先要做的事是选择一个 Lisp 实现。这对那些曾经使用诸如 Perl、Python、Visual
Basic (VB)、C# 和 Java 语言的人来说可能有些奇怪。Common Lisp
和这些语言的区别在于，Common Lisp 是标准化的——既不像 Perl 和 Python
那样是由善良的专制者所控制的某个实现，也不像 VB、C# 和 Java
那样是由某个公司控制的公认的实现。任何打算阅读标准并实现该语言的人都可以自由地这样做。更进一步，对标准的改动必须在标准化组织美国国家标准化协会（ANSI）所控制的程序下进行。这一设计可以避免任何个体（例如某个厂商）任意修改标准。这样，Common
Lisp 标准就是 Common Lisp 供应商和 Common Lisp
程序员之间的一份协议。这份协议告诉你，如果所写的程序按照标准里描述的方式使用了某些语言特性，那么就可以指望程序在任何符合标准的实现里也能产生同样的行为。

On the other hand, the standard may not cover everything you may want
to do in your programs--some things were intentionally left
unspecified in order to allow continuing experimentation by
implementers in areas where there wasn't consensus about the best way
for the language to support certain features. So every implementation
offers some features above and beyond what's specified in the
standard. Depending on what kind of programming you're going to be
doing, it may make sense to just pick one implementation that has the
extra features you need and use that. On the other hand, if we're
delivering Lisp source to be used by others, such as libraries, you'll
want--as far as possible--to write portable Common Lisp. For writing
code that should be mostly portable but that needs facilities not
defined by the standard, Common Lisp provides a flexible way to write
code "conditionalized" on the features available in a particular
implementation. You'll see an example of this kind of code in Chapter
15 when we develop a simple library that smoothes over some
differences between how different Lisp implementations deal with
filenames.

另一方面，标准可能并没有覆盖程序所涉及的每个方面——某些方面故意没有定义，允许实现者在这些领域里继续探索：特定的语言风格尚未找到最佳的支持方式。因此每种实现都提供了一些依托并超越标准所规定范围的特性。根据你正打算进行的编程类型，有时选择一种带有所需的额外特性的特定实现也是合理的。另一方面，如果我们正在向其他人交付Lisp源代码，例如库，那么你将需要尽可能地编写可移植的
Common Lisp。对于编写那些大部分可移植的，但需要用到标准中没有定义的功能的代码，假如所要编写的代码大部分可以移植，但其中要用到一些标准未曾定义过的功能。Common
Lisp 提供的灵活方式可以编写出完全依赖于特定实现中某些特性的代码。你将在第 15
章看到这样的代码，在那里我们将开发一个简单的库来消除不同
Lisp 实现在对待文件名上的区别。

For the moment, however, the most important characteristic of an
implementation is whether it runs on our favorite operating
system. The folks at Franz, makers of Allegro Common Lisp, are making
available a trial version of their product for use with this book that
runs on Linux, Windows, and OS X. Folks looking for an open-source
implementation have several options. SBCL is a high-quality
open-source implementation that compiles to native code and runs on a
wide variety of Unixes, including Linux and OS X. SBCL is derived from
CMUCL, which is a Common Lisp developed at Carnegie Mellon
University, and, like CMUCL, is largely in the public domain, except a
few sections licensed under Berkeley Software Distribution (BSD) style
licenses. CMUCL itself is another fine choice, though SBCL tends to be
easier to install and now supports 21-bit Unicode. For OS X users,
OpenMCL is an excellent choice--it compiles to machine code, supports
threads, and has quite good integration with OS X's Carbon and Cocoa
toolkits. Other open-source and commercial implementations are
available. See Chapter 32 for resources from which you can get more
information.

但就目前而论，一个 Lisp 实现最重要的特点应该在于它能否运行在我们所喜爱的操作系统上。Franz
的 Allegro Common Lisp 开发者们提供了一个可用于本书的试用版产品，它能够在
Linux、Windows 和 Mac OS X 上运行。试图寻找开源实现的人们有几种选择。SBCL（Steel
Bank Common Lisp）是一个高质量的开源实现，它将程序编译成原生代码并且可以运行在广泛的
Unix 平台上，包括 Linux 和 Mac OS X。SBCL 来源于 CMUCL（CMU Common
Lisp）——最早由卡内基梅隆大学开发的一种 Common Lisp，并且像 CMUCL
那样，大部分源代码都处于公共域（public domain），只有少量代码采用伯克利软件分发（BSD）风格的协议。CMUCL
本身也是个不错的选择，尽管 SBCL 更容易安装并且现在支持 21 位 Unicode。而对于
Mac OS X 用户来说，OpenMCL 是一个极佳的选择，它可编译到机器码、支持线程，并且可以跟
Mac OS X 的 Carbon 和 Cocoa
工具箱很好地集成。还有其他的开源和商业实现，参见第 32 章以获取更多相关资源的信息。

All the Lisp code in this book should work in any conforming Common
Lisp implementation unless otherwise noted, and SLIME will smooth out
some of the differences between implementations by providing us with a
common interface for interacting with Lisp. The output shown in this
book is from Allegro running on GNU/Linux; in some cases, other Lisp's
may generate slightly different error messages or debugger output.

除非特别说明，本书中的所有 Lisp 代码都应该可以工作在任何符合标准的 Common
Lisp 实现上，而 SLIME 通过为我们提供一个与 Lisp
交互的通用接口也可以消除不同实现之间的某些差异。本书里给出的程序输出来自运行在
GNU/Linux 上的 Allegro 平台。在某些情况下，其他
Lisp 可能会产生稍有不同的错误信息或调试输出。
