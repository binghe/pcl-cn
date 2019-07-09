# The Mighty **LOOP**（强大的 **LOOP**）

For the simple cases you have **DOLIST** and **DOTIMES**. And if they don't
suit your needs, you can fall back on the completely general DO. What
more could you want?

简单的情形可以使用 **DOLIST** 和
**DOTIMES**。但如果它们不符合需要，就需要退而使用完全通用的
**DO**。不然还能怎样？

Well, it turns out a handful of looping idioms come up over and over
again, such as looping over various data structures: lists, vectors,
hash tables, and packages. Or accumulating values in various ways
while looping: collecting, counting, summing, minimizing, or
maximizing. If you need a loop to do one of these things (or several
at the same time), the **LOOP** macro may give you an easier way to
express it.

然而，结果是有少量的循环用法一次又一次地产生出来，例如在多种数据结构上的循环：列表、向量、哈希表和包，或是在循环时以多种方式来集聚值：收集、计数、求和、最小化和最大化。如果需要用宏来做其中的一件事（或同时几件），那么
**LOOP** 宏可以提供一种更容易表达的方式。

The **LOOP** macro actually comes in two flavors--simple and extended. The
simple version is as simple as can be--an infinite loop that doesn't
bind any variables. The skeleton looks like this:

**LOOP** 宏事实上有两大类——简化的和扩展的。简化的版本极其简单，就是一个不绑定任何变量的无限循环。其框架看起来像这样：

```lisp
(loop
  body-form*)
```

The forms in body are evaluated each time through the loop, which will
iterate forever unless you use **RETURN** to break out. For example, you
could write the previous **DO** loop with a simple **LOOP**.

主体形式在每次通过循环时都将被求值，整个循环将不停地迭代，直到使用
**RETURN** 来进行中止。例如，可以使用一个简化的 **LOOP** 来写出前面的
**DO** 循环：

```lisp
(loop
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting~%")
  (sleep 60))
```

The extended **LOOP** is quite a different beast. It's distinguished by
the use of certain loop keywords that implement a special-purpose
language for expressing looping idioms. It's worth noting that not all
Lispers love the extended **LOOP** language. At least one of Common Lisp's
original designers hated it. **LOOP**'s detractors complain that its
syntax is totally un-Lispy (in other words, not enough
parentheses). **LOOP**'s fans counter that that's the point: complicated
looping constructs are hard enough to understand without wrapping them
up in **DO**'s cryptic syntax. It's better, they say, to have a slightly
more verbose syntax that gives you some clues what the heck is going
on.

而扩展的 **LOOP**
则是一个完全不同的庞然大物。它以使用某些循环关键字来实现一种用于表达循环用法的通用。值得注意的是，并非所有的
Lisp 程序员都喜爱扩展的 **LOOP** 语言。至少一位 Common Lisp
的最初设计者就很讨厌它。**LOOP** 的贬低者们抱怨它的语法是完全非
Lisp 化的（换句话说，没有足够的括号）。**LOOP**
的爱好者们则反驳说，问题在于复杂的循环构造，如果不将它们用 **DO**
那晦涩语法包装起来，它们将难于被人理解。所以他们认为最好用一种稍显冗长的语法来提供某些关于你正在做的事情的线索。

For instance, here's an idiomatic DO loop that collects the numbers from 1 to 10 into a list:

例如，下面是一个地道的 **DO** 循环，它将把从 1 到 10 的数字收集到一个列表中：

```lisp
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums)) ==> (1 2 3 4 5 6 7 8 9 10)
```

A seasoned Lisper won't have any trouble understanding that code--it's
just a matter of understanding the basic form of a DO loop and
recognizing the **PUSH**/**NREVERSE** idiom for building up a list. But it's
not exactly transparent. The **LOOP** version, on the other hand, is
almost understandable as an English sentence.

一个经验丰富的 Lisp 程序员将毫不费力地理解这些代码——只要理解一个 **DO**
循环的基本形式并且认识用于构建列表的 **PUSH**/**NREVERSE**
用法就可以了。但它并不是很直观。而它的 **LOOP**
版本理解起来就几乎可以像一个英语句子那样简单。

```lisp
(loop for i from 1 to 10 collecting i) ==> (1 2 3 4 5 6 7 8 9 10)
```

The following are some more examples of simple uses of **LOOP**. This sums
the first ten squares:

接下来是一些关于 **LOOP** 简单用法的例子。下例可以对前十个平方数求和：

```lisp
(loop for x from 1 to 10 summing (expt x 2)) ==> 385
```

This counts the number of vowels in a string:

这个用来统计一个字符串中元音字母的个数：

```lisp
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou")) ==> 11
```

This computes the eleventh Fibonacci number, similar to the **DO** loop
used earlier:

下面这个例子用来计算第 11 个斐婆那契数，它类似于前面使用 **DO** 循环的版本：

```lisp
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return  a))
```

The symbols `across`, `and`, `below`, `collecting`, `counting`, `finally`, `for`,
`from`, `summing`, `then`, and `to` are some of the loop keywords whose
presence identifies these as instances of the extended **LOOP**.

符号 `across`、`and`、`below`、`collecting`、`counting`、`finally`、`for`、`from`、`summing`、`then`
和 `to` 都是一些循环关键字，它们的存在表明当前正在使用扩展的 **LOOP**。

I'll save the details of **LOOP** for Chapter 22, but it's worth noting
here as another example of the way macros can be used to extend the
base language. While **LOOP** provides its own language for expressing
looping constructs, it doesn't cut you off from the rest of Lisp. The
loop keywords are parsed according to loop's grammar, but the rest of
the code in a **LOOP** is regular Lisp code.

第 22 章将介绍 **LOOP**
的细节，但目前值得注意的是，我们通过它可以再次看到，宏是如何被用于扩展基本语言的。尽管
**LOOP** 提供了它自己的语言用来表达循环构造，但它并没有抹杀
Lisp 的其他优势。虽然循环关键字是按照循环的语法来解析的，但一个
**LOOP** 中的其余代码都是正常的 Lisp 代码。

And it's worth pointing out one more time that while the **LOOP** macro is
quite a bit more complicated than macros such as **WHEN** or **UNLESS**, it is
just another macro. If it hadn't been included in the standard
library, you could implement it yourself or get a third-party library
that does.

另外，值得再次指出的是，尽管 **LOOP** 宏相比诸如 **WHEN** 或者 **UNLESS**
这样的宏复杂了许多，但它也只是另外一个宏而已。如果它没有被包括在标准库之中，你也可以自己实现它或是借助一个第三方库来实现它。

With that I'll conclude our tour of the basic control-construct
macros. Now you're ready to take a closer look at how to define your
own macros.

以上就是我们对基本控制构造宏的介绍。现在可以进一步了解如何定义自己的宏了。
