# Functions（函数）

After the rules of syntax and semantics, the three most basic
components of all Lisp programs are functions, variables and
macros. You used all three while building the database in Chapter 3,
but I glossed over a lot of the details of how they work and how to
best use them. I'll devote the next few chapters to these three
topics, starting with functions, which--like their counterparts in
other languages--provide the basic mechanism for abstracting, well,
functionality.

有了语法和语义规则以后，所有 Lisp
程序的三个最基本组成部分就是函数、变量和宏。在第 3
章里构建数据库时已经全部用到了这三个组件，但是我没有详细提及它们是如何工作的以及如何更好使用它们细节。接下来的几章将专门讲解这三个主题，先从函数开始——就像在其他语言里那样，函数提供了用于抽象和功能化的基本方法。

The bulk of Lisp itself consists of functions. More than three
quarters of the names defined in the language standard name
functions. All the built-in data types are defined purely in terms of
what functions operate on them. Even Lisp's powerful object system is
built upon a conceptual extension to functions, generic functions,
which I'll cover in Chapter 16.

Lisp
本身是由大量函数组成的。其语言标准中有超过四分之三的名字用于定义函数。所有内置的数据类型纯粹是用操作它们的函数来定义的。甚至连
Lisp 强大的对象系统也是构建在函数的概念性扩展——广义函数（generic
function）之上的，第 16 章将会介绍它们。

And, despite the importance of macros to The Lisp Way, in the end all
real functionality is provided by functions. Macros run at compile
time, so the code they generate--the code that will actually make up
the program after all the macros are expanded--will consist entirely
of calls to functions and special operators. Not to mention, macros
themselves are also functions, albeit functions that are used to
generate code rather than to perform the actions of the program.

并且，尽管宏对于 Lisp
风格有着重要的作用，但最终所有实际的功能还是由函数来提供的。宏运行在编译期，因此它们生成的代码，即当所有宏被展开后将实际构成程序的那些代码，将完全由对函数和特殊操作符的调用所构成。更不用说，宏本身也是函数了——尽管这种函数是用来生成代码，而不是用来完成实际的程序操作的。
