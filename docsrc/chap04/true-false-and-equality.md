# Truth, Falsehood, and Equality（真、假和等价）

Two last bits of basic knowledge you need to get under your belt are
Common Lisp's notion of truth and falsehood and what it means for two
Lisp objects to be "equal." Truth and falsehood are--in this
realm--straightforward: the symbol **NIL** is the only false value, and
everything else is true. The symbol **T** is the canonical true value and
can be used when you need to return a non-**NIL** value and don't have
anything else handy. The only tricky thing about **NIL** is that it's the
only object that's both an atom and a list: in addition to falsehood,
it's also used to represent the empty list. This equivalence between
**NIL** and the empty list is built into the reader: if the reader sees
`()`, it reads it as the symbol **NIL**. They're completely
interchangeable. And because **NIL**, as I mentioned previously, is the
name of a constant variable with the symbol **NIL** as its value, the
expressions `nil`, `()`, `'nil`, and `'()` all evaluate to the same thing--the
unquoted forms are evaluated as a reference to the constant variable
whose value is the symbol **NIL**, but in the quoted forms the **QUOTE**
special operator evaluates to the symbol directly. For the same
reason, both `t` and `'t` will evaluate to the same thing: the symbol **T**.

最后两个需要了解的基本知识是 Common Lisp 对于真和假的表示法以及两个
Lisp 对象“等价”的含义。真和假的含义在这里是直截了当的：符号 **NIL**
是唯一的假值，其他所有的都是真值。符号T是标准的真值，可被用于需要返回一个非
**NIL** 值却又没有其他值可用的情况。关于
**NIL**，唯一麻烦的一点是，它是唯一一个既是原子又是列表的对象：除了用来表示假以外，它还被用来表示空列表。 这种
**NIL** 和空列表的等价性被内置在读取器之中：如果读取器看到了 `()`，它将作为符号
**NIL** 读取它。它们是完全可以互换的。并且如同我前面提到的那样，因为 **NIL**
是一个以符号 **NIL** 作为其值的常值变量名，所以表达式 `nil`、`()`、
`'nil` 以及 `'()` 求值结果——未引用形式将被看出是对值为符号
**NIL** 的常值变量的引用未进行求值，而在引用形式中
**QUOTE** 特殊操作符将直接求解出符号 **NIL**。基于同样的理由，`t`
和 `'t` 的求值结果也完全相同：符号 **T**。

Using phrases such as "the same thing" of course begs the question of
what it means for two values to be "the same." As you'll see in future
chapters, Common Lisp provides a number of type-specific equality
predicates: `=` is used to compare numbers, **CHAR=** to compare characters,
and so on. In this section I'll discuss the four "generic" equality
predicates--functions that can be passed any two Lisp objects and will
return true if they're equivalent and false otherwise. They are, in
order of discrimination, **EQ**, **EQL**, **EQUAL**, and **EQUALP**.

使用诸如完全相同这样的术语，理所当然会引申出关值“等价”的这个问题上。在后面的章节里将会看到，Common
Lisp 提供了许多特定于类型的等价谓词：`=` 用来比较数字，**CHAR=**
用来比较字符，依此类推。本节将讨论四个“通用”等价谓词——这些函数可以被传入任何两个
Lisp 对象，然后当它们等价时返回真，否则返回假。按照介绍的顺序，它们是
**EQ**、**EQL**、**EQUAL** 和 **EQUALP**。

**EQ** tests for "object identity"--two objects are **EQ** if they're
identical. Unfortunately, the object identity of numbers and
characters depends on how those data types are implemented in a
particular Lisp. Thus, **EQ** may consider two numbers or two characters
with the same value to be equivalent, or it may not. Implementations
have enough leeway that the expression `(eq 3 3)` can legally evaluate
to either true or false. More to the point, `(eq x x)` can evaluate to
either true or false if the value of `x` happens to be a number or
character.

**EQ** 用来测试“对象标识”，只有当两个对象相同时才是 **EQ**
等价的。不幸的是，数字和字符的对象标识取决于这些数据类型在特定 Lisp
平台上实现的方式。带有相同值的两个数字或字符可能会被 **EQ**
认为是等价的也可能会是不等价的。语言实现者有足够的空间让表达式
`(eq 3 3)` 被合法地求值成无论真还是假。更有甚者
`(eq x x)` 可能有时求值为真，有时为假——如果 `x` 恰好是数字或字符。

Thus, you should never use **EQ** to compare values that may be numbers or
characters. It may seem to work in a predictable way for certain
values in a particular implementation, but you have no guarantee that
it will work the same way if you switch implementations. And switching
implementations may mean simply upgrading your implementation to a new
version--if your Lisp implementer changes how they represent numbers
or characters, the behavior of **EQ** could very well change as well.

因此，你应该永远不要将 **EQ**
用于比较可能是数字或字符的值上。在个别实现的特定值上，它可能会以一种可预测的方式工作，但如果切换语言实现，则它将不保证以相同的方式工作。而切换实现可能意味着只是简单地把实现升级到一个新版本——而如果
Lisp 实现者改变了表示数字或字符的方式，那么 **EQ** 的行为也将很有可能发生改变。

Thus, Common Lisp defines **EQL** to behave like **EQ** except that it also is
guaranteed to consider two objects of the same class representing the
same numeric or character value to be equivalent. Thus, `(eql 1 1)` is
guaranteed to be true. And `(eql 1 1.0)` is guaranteed to be false since
the integer value 1 and the floating-point value are instances of
different classes.

因此，Common Lisp 定义了 **EQL** 来获得与 **EQ**
相似的行为，除此之外，它也可以保证当相同类型的两个对象表示相同的数字或字符值时，它们是等价的。因此，`(eql 1 1)`
能被确保是真。而 `(eql 1 1.0)` 则被确保是假，因为整数值 1 和浮点数 1.0 是不同类型的对象。

There are two schools of thought about when to use **EQ** and when to use
**EQL**: The "use **EQ** when possible" camp argues you should use **EQ** when you
know you aren't going to be com-paring numbers or characters because
(a) it's a way to indicate that you aren't going to be comparing
numbers or characters and (b) it will be marginally more efficient
since **EQ** doesn't have to check whether its arguments are numbers or
characters.

关于何时使用 **EQ** 以及何时使用 **EQL**，这里有两种观点：“凡有可能就用
**EQ**” 阵营认为，当知道不存在比较数字或字符时就应该使用
**EQ**，他们认为 (a) 这是一种说明你不在比较数字或字符的方式，以及
(b) 因为 **EQ** 不需要检查它的参数是否为数字或字符，所以它将会稍微更有效率。

The "always use **EQL**" camp says you should never use **EQ** because (a) the
potential gain in clarity is lost because every time someone reading
your code--including you--sees an **EQ**, they have to stop and check
whether it's being used correctly (in other words, that it's never
going to be called upon to compare numbers or characters) and (b) that
the efficiency difference between **EQ** and **EQL** is in the noise compared
to real performance bottlenecks.

但 “总是使用 **EQL**” 阵营则认为永远不该使用
**EQ**，因为 (a) 每次有人（包括你自己在内）阅读你的代码时看到了一个
**EQ**，就得停下来检查它是否被正确使用了（换句话说，它永远不该被用来比较数字或字符），这样就会损失潜在的代码清晰性，以及
(b) **EQ** 和 **EQL** 之间的效率差异相比于实际的性能瓶颈来说微不足道。

The code in this book is written in the "always use **EQL**" style.

本书中的代码是以 “总是使用 **EQL**” 风格写成的。

The other two equality predicates, **EQUAL** and **EQUALP**, are general in
the sense that they can operate on all types of objects, but they're
much less fundamental than **EQ** or **EQL**. They each define a slightly less
discriminating notion of equivalence than **EQL**, allowing different
objects to be considered equivalent. There's nothing special about the
particular notions of equivalence these functions implement except
that they've been found to be handy by Lisp programmers in the
past. If these predicates don't suit your needs, you can always define
your own predicate function that compares different types of objects
in the way you need.

另外两个等价谓词 **EQUAL** 和 **EQUALP**
更为通用，因为它们可以操作在所有类型的对象上，但又不像 **EQ** 或
**EQL** 那样基础。它们每个都定义了相比 **EQL**
稍微宽松一些的等价性，允许认为不同的对象是等价的。除了它们曾经被过去的 Lisp
程序员认为是有用的之外。如果由这些函数所实现的特殊含义的等价性没有什么特别的，这些谓词不能满足需要，也可以自己定义谓词函数，以自己所需方式来比较不同类型对象。

**EQUAL** loosens the discrimination of **EQL** to consider lists equivalent
if they have the same structure and contents, recursively, according
to **EQUAL**. **EQUAL** also considers strings equivalent if they contain the
same characters. It also defines a looser definition of equivalence
than **EQL** for bit vectors and pathnames, two data types I'll discuss in
future chapters. For all other types, it falls back on **EQL**.

**EQUAL** 相比 **EQL**
的宽松之处在于,它将在归相上具有同结构和内容的列表视为等价。**EQUAL**
也认为含有相同字符的字符串是等价的，它对于位向量和路径名也定义了比 **EQL**
更加宽松的等价性，我将在未来的章节里讨论这两个数据类型。对于所
有其他类型，它回退到 **EQL** 的水平上。

**EQUALP** is similar to **EQUAL** except it's even less discriminating. It
considers two strings equivalent if they contain the same characters,
ignoring differences in case. It also considers two characters
equivalent if they differ only in case. Numbers are equivalent under
**EQUALP** if they represent the same mathematical value. Thus,
`(equalp 1 1.0)`
is true. Lists with **EQUALP** elements are **EQUALP**; likewise,
arrays with **EQUALP** elements are **EQUALP**.
As with **EQUAL**, there are a few
other data types that I haven't covered yet for which **EQUALP** can
consider two objects equivalent that neither **EQL** nor **EQUAL** will. For
all other data types, **EQUALP** falls back on **EQL**.

**EQUALP** 甚至更加宽松它和 **EQUAL**
是相似的。它在考察两个含有相同字符的字符串的等价性时忽略了大小写的区别。它还认为如果两个字符只在大小写上有区别，那么它们就是等价的。只要数字表示相同的数学意义上的值，它们
**EQUALP** 下面就是等价的。因此，`(equalp 1 1.0)`
是真的。由 **EQUALP**
等价的元素所组成的列表也是 **EQUALP**
等价的。同样地，带有 **EQUALP** 元素的数组也是 **EQUALP**
等价的。和 **EQUAL** 一样，还有一些我尚未涉及的其他数据类型，**EQUALP**
可认为两个对象是等价的，但 **EQL** 或
**EQUAL** 则不会。对于所有的其他数据类型，**EQUALP** 回退到 **EQL** 的水平上。
