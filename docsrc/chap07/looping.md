# Looping（循环）

Control constructs are the other main kind of looping
constructs. Common Lisp's looping facilities are--in addition to being
quite powerful and flexible--an interesting lesson in the
have-your-cake-and-eat-it-too style of programming that macros
provide.

循环结构是另外一类主要的控制结构。Common Lisp
的循环机制，除了更加强大和灵活以外，还是一门关于宏所提供的
“鱼和熊掌兼得” 的编程风格的有趣课程。

As it turns out, none of Lisp's 25 special operators directly support
structured looping. All of Lisp's looping control constructs are
macros built on top of a pair of special operators that provide a
primitive `goto` facility. Like many good abstractions, syntactic or
otherwise, Lisp's looping macros are built as a set of layered
abstractions starting from the base provided by those two special
operators.

初看起来，Lisp 的 25
个特殊操作符中没有一个能够直接支持结构化循环，所有的 Lisp
循环控制构造都是构建在一对提供原生 `goto`
机制的特殊操作符之上的宏。和许多好的抽象或句法等一样，Lisp
的循环宏构建在以那两个特殊操作符为基础的一组分层抽象之上。

At the bottom (leaving aside the special operators) is a very general
looping construct, **DO**. While very powerful, **DO** suffers, as do many
general-purpose abstractions, from being overkill for simple
situations. So Lisp also provides two other macros, **DOLIST** and
**DOTIMES**, that are less flexible than **DO** but provide convenient support
for the common cases of looping over the elements of a list and
counting loops. While an implementation can implement these macros
however it wants, they're typically implemented as macros that expand
into an equivalent **DO** loop. Thus, **DO** provides a basic structured
looping construct on top of the underlying primitives provided by
Common Lisp's special operators, and **DOLIST** and **DOTIMES** provide two
easier-to-use, if less general, constructs. And, as you'll see in the
next chapter, you can build your own looping constructs on top of **DO**
for situations where **DOLIST** and **DOTIMES** don't meet your needs.

最底层（不考虑特殊操作符）是一个非常通用的循环构造
**DO**。尽管非常强大，但 **DO**
和许多其他的通用抽象一样，在应用于简单情形时显得过于复杂。因此
Lisp 还提供了另外两个宏，**DOLIST** 和 **DOTIMES**。它们不像
**DO** 那样灵活，但却提供了对于常见的在列表元素上循环和计数循环的便利支持。尽管一个实现可以用任何方式来实现这些宏，但它们被典型实现为展开到等价
**DO** 循环的宏。因此，在由 Common Lisp
特殊操作符所提供的底层原语之上，**DO**
提供了一种基本的结构化循环构造，而 **DOLIST** 和 **DOTIMES**
则提供了两种易用却不那么通用的构造。并且如同在下一章将看到的那样，对于那些
**DOLIST** 和 **DOTIMES** 无法满足需要的情形，还可以在 **DO**
之上构建自定义的循环构造。

Finally, the **LOOP** macro provides a full-blown mini-language for
expressing looping constructs in a non-Lispy, English-like (or at
least Algol-like) language. Some Lisp hackers love **LOOP**; others hate
it. **LOOP**'s fans like it because it provides a concise way to express
certain commonly needed looping constructs. Its detractors dislike it
because it's not Lispy enough. But whichever side one comes down on,
it's a remarkable example of the power of macros to add new constructs
to the language.

最后，**LOOP** 宏提供了一种成熟的微型语言，它用一种非 Lisp
的类似英语（或至少类似 Algol）的语言来表达循环构造。一些 Lisp 黑客热爱
**LOOP**，其他人则讨厌它。**LOOP**
爱好者们喜欢它是因为它用了一种简洁的方式来表达特定的常用循环构造。而贬低者们不喜欢它则是因为它不太像
Lisp。但无论你倾向于哪一方，**LOOP**
本身都是一个为语言增加新构造的宏展示其强大威力的突出示例。
