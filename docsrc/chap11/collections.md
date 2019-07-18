# Collections（集合）

Like most programming languages, Common Lisp provides standard data
types that collect multiple values into a single object. Every
language slices up the collection problem a little bit differently,
but the basic collection types usually boil down to an integer-indexed
array type and a table type that can be used to map more or less
arbitrary keys to values. The former are variously called arrays,
lists, or tuples; the latter go by the names hash tables, associative
arrays, maps, and dictionaries.

和多数编程语言一样，Common Lisp
也提供了能将多个值收集到单一对象的标准数据类型。每一种语言在处理集合问题上都稍有不同，但基本的集合类型通常都可归结为一个整数索引的数组类型以及一个可将或多或少的任意关键字映射到值上的表类型。前者被分别称为数组（array）、列表（list）或元组（tuple），后者被命名为哈希表（hash table）、关联数组（associative array）、映射表（map）和字典（dictionary）。

Lisp is, of course, famous for its list data structure, and most Lisp
books, following the ontogeny-recapitulates-phylogeny principle of
language instruction, start their discussion of Lisp's collections
with lists. However, that approach often leads readers to the mistaken
conclusion that lists are Lisp's only collection type. To make matters
worse, because Lisp's lists are such a flexible data structure, it is
possible to use them for many of the things arrays and hash tables are
used for in other languages. But it's a mistake to focus too much on
lists; while they're a crucial data structure for representing Lisp
code as Lisp data, in many situations other data structures are more
appropriate.

当然，Lisp
以其列表数据结构闻名于世，而多数遵循了语言用法的进化重演（ontogeny-recapitulates-phylogeny）原则的
Lisp 教材也都从对基于列表的 Lisp
集合的讨论开始。尽管如此，这一观点通常导致读者错误地推论出列表是
Lisp 的唯一集合类型。更糟糕的是，因为 Lisp
的列表是如此灵活的数据结构，它可被用于许多其他语言使用数组和哈希表的场合。但是将注意力过于集中在列表是上错误的，尽管它是一种将
Lisp 代码作为 Lisp
数据来表达的关键数据结构，但在许多场合其他数据结构更合适。

To keep lists from stealing the show, in this chapter I'll focus on
Common Lisp's other collection types: vectors and hash tables.
However, vectors and lists share enough characteristics that Common
Lisp treats them both as subtypes of a more general abstraction, the
sequence. Thus, you can use many of the functions I'll discuss in this
chapter with both vectors and lists.

为了避免让列表过于出风头，在本章里我将集中在 Common Lisp
的其他集合类型上：向量和哈希表。 尽管如此，向量和列表共享了许多特征，因此 Common
Lisp 将它们都视为一个更通用的抽象即序列的子类型。因此，你可以将本章我所讨论的许多函数同时用在向量和列表上。

