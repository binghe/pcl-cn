# Wrapping Up（总结）

Now, an interesting thing has happened. You removed duplication and
made the code more efficient _and_ more general at the same time. That's
often the way it goes with a well-chosen macro. This makes sense
because a macro is just another mechanism for creating
abstractions--abstraction at the syntactic level, and abstractions are
by definition more concise ways of expressing underlying
generalities. Now the only code in the mini-database that's specific
to CDs and the fields in them is in the `make-cd`, `prompt-for-cd`, and
`add-cd` functions. In fact, our new `where` macro would work with any
plist-based database.

现在，有趣的事情发生了。你不但去除了重复，而且还使得代码更有效且更通用了。这通常就是正确选用宏所达到的效果。这件事合乎逻辑，是因为宏只不过是另一种创建抽象的手法——词法层面的抽象，以及按照定义通过更简明地表达底层一般性的方式所得到的抽象。现在这个微型数据库的代码中只有
`make-cd`、`prompt-for-cd` 以及 `add-cd`
函数是特定于 CD 及其字段的。事实上，新的 `where` 宏可以用在任何基于
plist 的数据库上。

However, this is still far from being a complete database. You can
probably think of plenty of features to add, such as supporting
multiple tables or more elaborate queries. In Chapter 27 we'll build
an MP3 database that incorporates some of those features.

尽管如此，它距离一个完整的数据库仍很遥远。你可能会想到还有大量需要增加的特性，包括支持多表或是更复杂的查询。第
27 章将建立一个具备这些特性的 MP3 数据库。

The point of this chapter was to give you a quick introduction to just
a handful of Lisp's features and show how they're used to write code
that's a bit more interesting than "hello, world." In the next chapter
we'll begin a more systematic overview of Lisp.

本章的要点在于快速介绍少量 Lisp 特性，展示如何用它们编写出比 “hello,
world” 更有趣一点儿的代码。在下一章里，我们将对 Lisp 做一个更加系统的概述。
