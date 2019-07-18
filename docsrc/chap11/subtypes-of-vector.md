# Subtypes of Vector（向量的子类型）

All the vectors you've dealt with so far have been general vectors
that can hold any type of object. It's also possible to create
specialized vectors that are restricted to holding certain types of
elements. One reason to use specialized vectors is they may be stored
more compactly and can provide slightly faster access to their
elements than general vectors. However, for the moment let's focus on
a couple kinds of specialized vectors that are important data types in
their own right.

目前为止，你处理的所有向量都是可以保存任意类型对象的通用向量。也可以创建特化的向量使其仅限于保存特定类型的元素。使用特化向量的理由之一是，它们可以更加紧凑地存储，并且可以比通用向量提供对其元素更快速的访问。不过目前我们将集中介绍几类特化向量，它们本身就是重要的数据类型。

One of these you've seen already--strings are vectors specialized to
hold characters. Strings are important enough to get their own
read/print syntax (double quotes) and the set of string-specific
functions I discussed in the previous chapter. But because they're
also vectors, all the functions I'll discuss in the next few sections
that take vector arguments can also be used with strings. These
functions will fill out the string library with functions for things
such as searching a string for a substring, finding occurrences of a
character within a string, and more.

其中一类你已经见过了，这就是字符串，它是特定用来保存字符的向量。字符串特别重要，以至于它们到有自己的读写语法（双引号）和一组特定于字符串的函数前一章已讨论过。但因为它们也是向量，所有接下来几节里所讨论的接受向量实参的函数也可以用在字符串上。这些函数将为字符串函数库带来新的功能，例如用一个子串来搜索字符串，查找一个字符在字符串中出现的次数，等等。

Literal strings, such as "foo", are like literal vectors written with
the `#()` syntax--their size is fixed, and they must not be
modified. However, you can use **MAKE-ARRAY** to make resizable strings by
adding another keyword argument, `:element-type`. This argument takes a
type descriptor. I won't discuss all the possible type descriptors you
can use here; for now it's enough to know you can create a string by
passing the symbol **CHARACTER** as the `:element-type` argument. Note that
you need to quote the symbol to prevent it from being treated as a
variable name. For example, to make an initially empty but resizable
string, you can write this:

诸如 “foo”
这样的字面字符串，和那些用 `#()`
语法写成的字面向量一样，其大小都是固定的，并且它们根本不能被修改。但你可以用 **MAKE-ARRAY**
通过添加另一个关键字参数 `:element-type`
来创建变长字符串。该参数接受一个类型描述符。我将不会介绍你可以在这里使用的所有可能的类型描述符，目前只需知道你可以通过传递符号
**CHARACTER** 作为 `:element-type`
来创建字符串。注意，你需要引用该符号以避免它被视为一个变量名。例如，创建一个初始为空但却变长的字符串如下所示：

```lisp
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) ==> ""
```

Bit vectors--vectors whose elements are all zeros or ones--also get
some special treatment. They have a special read/print syntax that
looks like `#*00001111` and a fairly large library of functions, which I
won't discuss, for performing bit-twiddling operations such as
"anding" together two bit arrays. The type descriptor to pass as the
:element-type to create a bit vector is the symbol **BIT**.

位向量是元素全部由 0 或 1
所组成的向量，它也得到一些特殊对待。它们有一个特别的读/写语法，看起来像
`#*00001111`，另外还有一个相对巨大的函数库。这些函数可用于按位操作，例如将两个位数组
“与” 在一起（这里不会介绍）。用来创建一个位向量传递给 `:element-type`
的类型描述符是符号 **BIT**。

