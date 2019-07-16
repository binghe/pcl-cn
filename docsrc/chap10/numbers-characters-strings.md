# Numbers, Characters, and Strings（数字、字符和字符串）

While functions, variables, macros, and 25 special operators provide
the basic building blocks of the language itself, the building blocks
of your programs will be the data structures you use. As Fred Brooks
observed in *The Mythical Man-Month*, "Representation is the essence
of programming."

尽管函数、变量、宏和
25 个特殊操作符组成了语言本身的基本构造单元，你的程序的基本构造单元将是你所使用的数据结构。正如
Fred Brooks 在 *The Mythical Man-Month*
里提到的，“编程的本质在于表示。”

Common Lisp provides built-in support for most of the data types
typically found in modern languages: numbers (integer, floating point,
and complex), characters, strings, arrays (including multidimensional
arrays), lists, hash tables, input and output streams, and an
abstraction for portably representing filenames. Functions are also a
first-class data type in Lisp--they can be stored in variables, passed
as arguments, returned as return values, and created at runtime.

Common Lisp
为现代语言中常见的大多数数据类型都提供了内置支持：数字（整数、浮点数和复数）、字符 、字符串、数组（包括多维数组）、列表、哈希表、输入和输出流以及一种可移植地表示文件名的抽象。函数在
Lisp
中也是第一类（fist-class）数据类型。它们可以被保存在变量中，可以作为实参传递，也可以作为返回值返回以及在运行期创建。

And these built-in types are just the beginning. They're defined in
the language standard so programmers can count on them being available
and because they tend to be easier to implement efficiently when
tightly integrated with the rest of the implementation. But, as you'll
see in later chapters, Common Lisp also provides several ways for you
to define new data types, define operations on them, and integrate
them with the built-in data types.

而这些内置类型仅仅是开始。它们被定义在语言标准中，因此程序员们可以依赖于它们的存在，并且也因为它们可以跟语言的其余部分紧密集成，从而使其可以更容易地高效实现。但正如你将在后续章节里看到的那样，Common
Lisp 另外还提供了几种定义新的数据类型的方式，并能定义对其的操作，并能将它们与内置数据类型集成起来。

For now, however, you can start with the built-in data types. Because
Lisp is a high-level language, the details of exactly how different
data types are implemented are largely hidden. From your point of view
as a user of the language, the built-in data types are defined by the
functions that operate on them. So to learn a data type, you just have
to learn about the functions you can use with it. Additionally, most
of the built-in data types have a special syntax that the Lisp reader
understands and that the Lisp printer uses. That's why, for instance,
you can write strings as `"foo"`; numbers as `123`, `1/23`, and `1.23`; and
lists as `(a b c)`. I'll describe the syntax for different kinds of
objects when I describe the functions for manipulating them.

但目前将先从内置数据类型开始讲起。因为 Lisp
是一种高级语言，不同的数据类型的具体实现细节在很大程度上是隐藏的。从语言用户的角度来看，内置数据类型是由操作它们的函数所定义的。因此为了学习一个数据类型，你只需学会那些与之一起使用的函数就行了。另外，多数内置数据类型都具有
Lisp 读取器所理解并且 Lisp
打印器可使用的特殊语法。所以你才能将字符串写成
`"foo"`，将数字写成 `123`、`1/23` 和 `1.23`，以及把列表写成
`(a b c)`。我将在描述操作它们的函数时具体描述不同对象的语法。

In this chapter, I'll cover the built-in "scalar" data types: numbers,
characters, and strings. Technically, strings aren't true scalars--a
string is a sequence of characters, and you can access individual
characters and manipulate strings with a function that operates on
sequences. But I'll discuss strings here because most of the
string-specific functions manipulate them as single values and also
because of the close relation between several of the string functions
and their character counterparts.

本章将介绍内置的 “标量”
数据类型：数字、字符和字符串。从技术上来讲，字符串并不是真正的标量。字符串是字符的序列，你可以访问单独的字符并使用一个操作在序列上的函数来处理该字符串。但我在这里讨论字符串则是因为多数字符串的相关函数会将它们作为单一值来处理，同时也是因为某些字符串函数与它们的字符组成部分之间有着紧密的关系。

