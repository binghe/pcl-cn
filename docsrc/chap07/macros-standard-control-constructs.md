# Macros: Standard Control Constructs（宏：标准控制结构）

While many of the ideas that originated in Lisp, from the conditional
expression to garbage collection, have been incorporated into other
languages, the one language feature that continues to set Common Lisp
apart is its macro system. Unfortunately, the word macro describes a
lot of things in computing to which Common Lisp's macros bear only a
vague and metaphorical similarity. This causes no end of
misunderstanding when Lispers try to explain to non-Lispers what a
great feature macros are. To understand Lisp's macros, you really
need to come at them fresh, without preconceptions based on other
things that also happen to be called macros. So let's start our
discussion of Lisp's macros by taking a step back and looking at
various ways languages support extensibility.

尽管起源于Lisp
的许多编程思想（从条件表达式到垃圾收集）都已经被吸取进其他语言，但
Lisp 的宏系统却始终使它保持了在语言风格上的独特性。不幸的是，宏这个字虽然在计算领域可以描述很多东西，但和
Common Lisp的宏相比，它们仅具有模糊和大致的相似性。当
Lisp 程序员们试图向非 Lisp
程序员解释宏这种特性的伟大之处时，这种相似性导致了无休止的误解。要想理解
Lisp 的宏，就真的需要重新看待它，不能带有任何基于其他碰巧叫做宏的概念所带来的成见。现在先退一步，从观察各种语言支持扩展的不同方式讨论
Lisp 宏。

All programmers should be used to the idea that the definition of a
language can include a standard library of functionality that's
implemented in terms of the "core" language--functionality that could
have been implemented by any programmer on top of the language if it
hadn't been defined as part of the standard library. C's standard
library, for instance, can be implemented almost entirely in portable
C. Similarly, most of the ever-growing set of classes and interfaces
that ship with Java's standard Java Development Kit (JDK) are written
in "pure" Java.

所有的程序员应该都熟知这么一种观点：“编程语言” 的定义包括一个使用 “核心”
语言实现的标准功能库——如果某些功能没有定义在标准库中，那么它们可能已经被程序员实现在语言中了。只要它还没有被定义成标准库的一部分。例如，C
的标准库就差不多可以完全用可移植的 C
来实现。类似地，Java 的标准开发包（JDK）中所提供的不断改进的类和接口集合也是用
“纯” Java 编写的。

One advantage of defining languages in terms of a core plus a standard
library is it makes them easier to understand and implement. But the
real benefit is in terms of expressiveness--since much of what you
think of as "the language" is really just a library--the language is
easy to extend. If C doesn't have a function to do some thing or
another that you need, you can write that function, and now you have a
slightly richer version of C. Similarly, in a language such as Java or
Smalltalk where almost all the interesting parts of the "language" are
defined in terms of classes, by defining new classes you extend the
language, making it more suited for writing programs to do whatever it
is you're trying to do.

使用核心加上标准库的方式来定义语言的优势在于易于理解和实现。但真正的好处在于其可表达性——由于所认为的 “该语言”
很大程度上其实是一个库，因此很容易对其进行扩展。如果
C 语言中不含有所需的用来做某件事的一个函数，那就可以写出这个函数，然后就得到了一个特性稍微丰富一点的
C 版本。类似地，在诸如 Java 或 Smalltalk
这类几乎全部的有趣部分都是由类来定义的语言里，通过定义新的类就可以扩展该语言，使其更适用于编写你正试图编写的无论什么程序。

While Common Lisp supports both these methods of extending the
language, macros give Common Lisp yet another way. As I discussed
briefly in Chapter 4, each macro defines its own syntax, determining
how the s-expressions it's passed are turned into Lisp forms. With
macros as part of the core language it's possible to build new
syntax--control constructs such as WHEN, DOLIST, and LOOP as well as
definitional forms such as DEFUN and DEFPARAMETER--as part of the
"standard library" rather than having to hardwire them into the
core. This has implications for how the language itself is
implemented, but as a Lisp programmer you'll care more that it gives
you another way to extend the language, making it a better language
for expressing solutions to your particular programming problems.

尽管 Common Lisp
支持所有这些扩展语言的方法，宏还提供了另一种方式。如同第 4
章所概述的那样，每个宏都定义了自己的语法，它们能够决定那些被传递的
S-表达式如何被转换成 Lisp
形式。核心语言有了宏，就有可能构造出新的语法——诸如 **WHEN**、**DOLIST**
和 **LOOP** 这样的控制构造以及 **DEFUN** 和
**DEFPARAMETER** 这样的定义形式，从而作为 “标准库”
的一部分而不是将其硬编码到语言核心。这已经牵涉到语言本身是如何实现的，但作为一个
Lisp 程序员你更关心的将是它所提供的另一种语言扩展式，使这些新语法可以使
Common Lisp 成为更好的用于表达特定编程问题解决方案的语言。

Now, it may seem that the benefits of having another way to extend the
language would be easy to recognize. But for some reason a lot of
folks who haven't actually used Lisp macros--folks who think nothing
of spending their days creating new functional abstractions or
defining hierarchies of classes to solve their programming
problems--get spooked by the idea of being able to define new
syntactic abstractions. The most common cause of macrophobia seems to
be bad experiences with other "macro" systems. Simple fear of the
unknown no doubt plays a role, too. To avoid triggering any
macrophobic reactions, I'll ease into the subject by discussing
several of the standard control-construct macros defined by Common
Lisp. These are some of the things that, if Lisp didn't have macros,
would have to be built into the language core. When you use them, you
don't have to care that they're implemented as macros, but they
provide a good example of some of the things you can do with macros.
In the next chapter, I'll show you how you can define your own macros.

那么，利用另一种方式来拓展语言的好处似乎是显而易见的。但出于某些原因，大量没有实际使用过
Lisp 宏的人，他们可以为了解决编程问题而日复一日地创建新的函数型抽象或定义类的层次体系，但却被这种可以定义新的句法抽象的思想给吓到了。通常，这种宏恐惧症的原因多半是来自学习其他“宏”系统时的不良经历。简单地对未知事物的恐惧无疑也是其中一部分原因。为了避免触发任何宏恐惧症反应，讨论将从
Common Lisp
所定义的几种标准控制构造宏开始，既而缓慢进入该主题。它们都是那些如果
Lisp 没有宏，就必须构造在语言核心里的东西。尽管在使用时不必关心它们是一种作为宏的实现，但它们的确可以很好地展示出宏的一些功用。下一章将说明如何定义你自己的宏。
