# They Called It LISP for a Reason: List Processing（LISP 名字的由来：列表处理）

Lists play an important role in Lisp--for reasons both historical and
practical. Historically, lists were Lisp's original composite data
type, though it has been decades since they were its only such data
type. These days, a Common Lisp programmer is as likely to use a
vector, a hash table, or a user-defined class or structure as to use a
list.

无论是出于历史还是实践的的原因，列表在 Lisp
中都扮演着重要的角色。在历史上，列表曾经是 Lisp
最初的复合数据类型，尽管很多年从实践上来它是这方面唯一的数据类型。现在，Lisp
程序员可能会使用向量、哈希表、用户自定义的类或者结构体来代替列表。

Practically speaking, lists remain in the language because they're an
excellent solution to certain problems. One such problem--how to
represent code as data in order to support code-transforming and
code-generating macros--is particular to Lisp, which may explain why
other languages don't feel the lack of Lisp-style lists. More
generally, lists are an excellent data structure for representing any
kind of heterogeneous and/or hierarchical data. They're also quite
lightweight and support a functional style of programming that's
another important part of Lisp's heritage.

从实践上来讲，由于列表对特定问题提供了极佳的解决方案，因此它们仍然能留在语言之中。其中有这样一个问题——如何将代码表示成数据从而支持代码转换和生成代码的宏，它就是特定于
Lisp 的，这就可以解释为什么其他语言没有感觉到因缺少 Lisp
式列表所带来的不便。更一般地讲，列表是用于表达任何异构和（或）层次数据的极佳数据结构。它们相当轻量并且支持函数式的编程风格，而这种编程风格也是
Lisp 传统的另一个重要方面。

Thus, you need to understand lists on their own terms; as you gain a
better understanding of how lists work, you'll be in a better position
to appreciate when you should and shouldn't use them.

因此，你需要更加深入地理解列表。一旦对列表的工作方式有了更加深刻的理解，你将会对如何适时地使用它们有更好的认识。

