# Macros: Defining Your Own（定义你自己的宏）

Now it's time to start writing your own macros. The standard macros I
covered in the previous chapter hint at some of the things you can do
with macros, but that's just the beginning. Common Lisp doesn't
support macros so every Lisp programmer can create their own variants
of standard control constructs any more than C supports functions so
every C programmer can write trivial variants of the functions in the
C standard library. Macros are part of the language to allow you to
create abstractions on top of the core language and standard library
that move you closer toward being able to directly express the things
you want to express.

现在可以开始编写自己的宏了。前一章里提及的标准宏暗示了可以用宏做到的某些事情，但这只是开始。相比
C 语言的函数可以让每个 C 程序员编写 C 标准库中的函数的简单变体而言，Common
Lisp 的宏也无非是可以让每个 Lisp
程序员可以创建他们自己的标准控制构造变体罢了。作为语言的一部分，宏能够用于在核心语言和标准库之上创建抽象，从而使你更直接地表达想表达的事物。

Perhaps the biggest barrier to a proper understanding of macros is,
ironically, that they're so well integrated into the language. In many
ways they seem like just a funny kind of function--they're written in
Lisp, they take arguments and return results, and they allow you to
abstract away distracting details. Yet despite these many
similarities, macros operate at a different level than functions and
create a totally different kind of abstraction.

具有讽刺意义的是，也许对于宏的正确理解，最大的障碍是它们已经很好地集成到了语言里。在许多方面，它们看起来只是一些有趣的函数——它们用
Lisp 写成，接受参数并返回结果。同时，它们允许你将那些分散注意力的细节抽象掉。尽管有这些相似性，但宏的操作层面却与函数不同，而且它还有着完全不同类型的抽象。

Once you understand the difference between macros and functions, the
tight integration of macros in the language will be a huge
benefit. But in the meantime, it's a frequent source of confusion for
new Lispers. The following story, while not true in a historical or
technical sense, tries to alleviate the confusion by giving you a way
to think about how macros work.

一旦理解了宏与函数之间的区别，你就会发现这门语言中宏的紧密集成所带来的巨大优势。但同时它也是经常导致新程序员困惑的主要原因。下面来讲个故事，尽管从历史或技术意义上来说都并不是真的，然而通过这种方式，你倒是可以思考一下宏的工作方式，以此来缓解一下困惑。
