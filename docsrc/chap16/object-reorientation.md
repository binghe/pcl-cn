# Object Reorientation: Generic Functions（重新审视面向对象：广义函数）

Because the invention of Lisp predated the rise of object-oriented
programming by a couple decades, new Lispers are sometimes surprised
to discover what a thoroughly object-oriented language Common Lisp
is. Common Lisp's immediate predecessors were developed at a time when
object orientation was an exciting new idea and there were many
experiments with ways to incorporate the ideas of object orientation,
especially as manifested in Smalltalk, into Lisp. As part of the
Common Lisp standardization, a synthesis of several of these
experiments emerged under the name Common Lisp Object System, or
CLOS. The ANSI standard incorporated CLOS into the language, so it no
longer really makes sense to speak of CLOS as a separate entity.

Lisp 的发明比面向对象编程的兴起早了几十年。新的 Lisp
程序员们有时会惊奇地发现，原来 Common Lisp
竟是一门非常彻底的面向对象语言。Common Lisp 之前的几个 Lisp
方言开发于面向对象还是一个崭新思想的年代，而那时有许多实验在探索将面向对象的思想（尤其是
Smalltalk 中所展现的形式）合并到 Lisp 中的方式。作为 Common Lisp
标准化过程的一部分，这些实验中的一些被合成在一起，以 Common Lisp
Object System (CLOS) 的名义出现。ANSI CL 标准将 CLOS
合并到了语言之中，因此单独提及 CLOS 就不再有任何实际意义了。

The features CLOS contributed to Common Lisp range from those that can
hardly be avoided to relatively esoteric manifestations of Lisp's
language-as-language-building-tool philosophy. Complete coverage of
all these features is beyond the scope of this book, but in this
chapter and the next I'll describe the bread-and-butter features and
give an overview of Common Lisp's approach to objects.

CLOS 为 Common Lisp
贡献的那些特性里既有必不可少的，也有相对难懂的 Lisp
“语言作为语言的构造工具”
这一哲学的具体表现。本书无法对所有这些特性全部加以介绍，但在本章和下一章里，我将描述其中最常用的特性，并给出关于
Common Lisp 对象的概述。

You should note at the outset that Common Lisp's object system offers
a fairly different embodiment of the principles of object orientation
than many other languages. If you have a deep understanding of the
fundamental ideas behind object orientation, you'll likely appreciate
the particularly powerful and general way Common Lisp manifests those
ideas. On the other hand, if your experience with object orientation
has been largely with a single language, you may find Common Lisp's
approach somewhat foreign; you should try to avoid assuming that
there's only one way for a language to support object orientation.2 If
you have little object-oriented programming experience, you should
have no trouble understanding the explanations here, though it may
help to ignore the occasional comparisons to the way other languages
do things.

你应当从一开始就注意到，Common Lisp
的对象系统体现了与许多其他语言相当不同的面向对象的原则。如果你能够深刻理解面向对象背后的基本思想，那么将会感谢
Common Lisp
在实现这些思想时所采用的强大和通用的方式。另一方面，如果你的面向对象经历很大程度上来自单一语言，那么你可能会发现
Common Lisp
的观点多少有些另类。你应当试图避免假设只存在一种方式令一门语言支持面向对象。 如果你几乎没有面向对象编程经验，那么也应当不难理解这里的解释，不过文中偶尔比较其他语言做同样事情的方式的内容，你就只好跳过不看了。
