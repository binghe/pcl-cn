# Removing Duplication and Winning Big（消除重复，获益良多）

So far all the database code supporting insert, select, update, and
delete, not to mention a command-line user interface for adding new
records and dumping out the contents, is just a little more than 50
lines. Total.

目前所有的数据库代码，支持插入、选择、更新，更不用说还有用来添加新记录和导出内容的命令行接口，只有
50 行多点儿。总共就这些。

Yet there's still some annoying code duplication. And it turns out you
can remove the duplication and make the code more flexible at the same
time. The duplication I'm thinking of is in the where function. The
body of the where function is a bunch of clauses like this, one per
field:

不过这里仍然有一些讨厌的代码重复，看来可以在消除重复的同时使代码更为灵活。我所考虑的重复出现在
`where` 函数里。`where`
的函数体是一堆像这样的子句，每字段一个：

```lisp
(if title (equal (getf cd :title) title) t)
```

Right now it's not so bad, but like all code duplication it has the
same cost: if you want to change how it works, you have to change
multiple copies. And if you change the fields in a CD, you'll have to
add or remove clauses to `where`. And `update` suffers from the same kind
of duplication. It's doubly annoying since the whole point of the
`where` function is to dynamically generate a bit of code that checks
the values you care about; why should it have to do work at runtime
checking whether title was even passed in?

眼下情况还不算太坏，但它的开销和所有的代码重复是一样的：如果想要改变它的行为，就不得不改动它的多个副本。并且如果改变了
CD 的字段，就必须添加或删除 `where` 的子句。而 `update`
也需要承担同样的重复。最令人讨厌的一点在于，`where`
函数的本意是去动态生成一点儿代码来检查你所关心的那些值，但为什么它非要在运行期来检查
`title` 参数是否被传递进来了呢？

Imagine that you were trying to optimize this code and discovered that
it was spending too much time checking whether `title` and the rest of
the keyword parameters to `where` were even set? If you really wanted
to remove all those runtime checks, you could go through a program and
find all the places you call where and look at exactly what arguments
you're passing. Then you could replace each call to where with an
anonymous function that does only the computation necessary. For
instance, if you found this snippet of code:

想象一下你正在试图优化这段代码，并且已经发现了它花费太多的时间检查
`title` 和 `where`
的其他关键字形参是否被设置了。如果真的想要移除所有这些运行期检查，则可以通过一个程序将所有这些调用
`where` 的位置以及究竟传递了哪些参数都找出来。然后就可以替换每一个对
`where` 的调用，使用一个只做必要比较的匿名函数。举个例子，如果发现这段代码：

```lisp
(select (where :title "Give Us a Break" :ripped t))
```

you could change it to this:

你可以将其改为：

```lisp
(select
 #'(lambda (cd)
     (and (equal (getf cd :title) "Give Us a Break")
          (equal (getf cd :ripped) t))))
```

Note that the anonymous function is different from the one that `where`
would have returned; you're not trying to save the call to `where` but
rather to provide a more efficient selector function. This anonymous
function has clauses only for the fields that you actually care about
at this call site, so it doesn't do any extra work the way a function
returned by where might.

注意到这个匿名函数跟 `where` 所返回的那个是不同的，你并非在试图节省对
`where` 的调用，而是提供了一个更有效率的选择器函数。这个匿名函数只带有在这次调用里实际关心的字段所对应的子句，所以它不会像
`where` 可能返回的函数那样做任何额外的工作。

You can probably imagine going through all your source code and fixing
up all the calls to where in this way. But you can probably also
imagine that it would be a huge pain. If there were enough of them,
and it was important enough, it might even be worthwhile to write some
kind of preprocessor that converts `where` calls to the code you'd write
by hand.

你可能会想象把所有的源代码都过一遍，并以这种方式修复所有对 `where`
的调用，但你也会想到这样做将是极其痛苦的。如果它们有足够多，足够重要，那么编写某种可以将
`where` 调用转化成你手写代码的预处理器就是非常值得的了。

The Lisp feature that makes this trivially easy is its macro system. I
can't emphasize enough that the Common Lisp macro shares essentially
nothing but the name with the text-based macros found in C and
C++. Where the C pre-processor operates by textual substitution and
understands almost nothing of the structure of C and C++, a Lisp macro
is essentially a code generator that gets run for you automatically by
the compiler. When a Lisp expression contains a call to a macro,
instead of evaluating the arguments and passing them to the function,
the Lisp compiler passes the arguments, unevaluated, to the macro
code, which returns a new Lisp expression that is then evaluated in
place of the original macro call.

使这件事变得极其简单的 Lisp 特性是它的宏（macro）系统。我必须反复强调，Common
Lisp 的宏和那些在 C 和 C++
里看到的基于文本的宏，从本质上讲，除了名字相似以外就再没有其他共同点了。C
预处理器操作在文本替换层面上，对 C 和 C++ 的结构几乎一无所知；而 Lisp
宏在本质上是一个由编译器自动为你运行的代码生成器。 当一个 Lisp
表达式包含了对宏的调用时，Lisp
编译器不再求值参数并将其传给函数，而是直接传递未经求值的参数给宏代码，后者返回一个新的
Lisp 表达式，在原先宏调用的位置上进行求值。

I'll start with a simple, and silly, example and then show how you can
replace the `where` function with a `where` macro. Before I can write this
example macro, I need to quickly introduce one new function: **REVERSE**
takes a list as an argument and returns a new list that is its
reverse. So `(reverse '(1 2 3))` evaluates to `(3 2 1)`. Now let's create
a macro.

我将从一个简单而荒唐的例子开始，然后说明你应该怎样把 `where` 函数替换成一个
`where` 宏。在开始写这个示例宏之前，我需要快速介绍一个新函数：**REVERSE**，它接受一个列表作为参数并返回一个逆序的新列表。因此
`(reverse '(1 2 3))` 的求值结果为 `(3 2 1)`。现在让我们创建一个宏：

```lisp
(defmacro backwards (expr) (reverse expr))
```

The main syntactic difference between a function and a macro is that
you define a macro with **DEFMACRO** instead of **DEFUN**. After that a macro
definition consists of a name, just like a function, a parameter list,
and a body of expressions, both also like a function. However, a macro
has a totally different effect. You can use this macro as follows:

函数和宏的主要词法差异在于你需要用 **DEFMACRO** 而不是 **DEFUN**
来定义一个宏。 除此之外，宏定义包括名字，就像函数那样，另外宏还有形参列表以及表达式体，这些也与函数一样。但宏却有着完全不同的效果。你可以像下面这样来使用这个宏：

```lisp
CL-USER> (backwards ("hello, world" t format))
hello, world
NIL
```

How did that work? When the REPL started to evaluate the `backwards`
expression, it recognized that `backwards` is the name of a macro. So it
left the expression `("hello, world" t format)` unevaluated, which is
good because it isn't a legal Lisp form. It then passed that list to
the `backwards` code. The code in `backwards` passed the list to **REVERSE**,
which returned the list `(format t "hello, world")`. `backwards` then
passed that value back out to the REPL, which then evaluated it in
place of the original expression.

它是怎么工作的？ REPL 开始求值这个 `backwards` 表达式时，它认识到 `backwards`
是一个宏名。因此它保持表达式 `("hello, world" t format)`
不被求值。这样正好，因为它不是一个合法的 Lisp 形式。REPL 随后将这个列表传给
`backwards` 代码。`backwards` 中的代码再将列表传给 **REVERSE**，后者返回列表
`(format t "hello, world")`。`backwards` 再将这个值传回给
REPL，然后对其求值以顶替最初表达式。

The backwards macro thus defines a new language that's a lot like
Lisp--just backward--that you can drop into anytime simply by wrapping
a backward Lisp expression in a call to the backwards macro. And, in a
compiled Lisp program, that new language is just as efficient as
normal Lisp because all the macro code--the code that generates the
new expression--runs at compile time. In other words, the compiler
will generate exactly the same code whether you write `(backwards
("hello, world" t format))` or `(format t "hello, world")`.

这样 `backwards` 宏就相当于定义了一个跟 Lisp
很像（只是反了一下）的新语言，你随时可以通过将一个逆序的
Lisp 表达式包装在一个对 `backwards` 宏的调用里来使用它。而且，在编译了的
Lisp 程序里，这种新语言的效率就跟正常 Lisp
一样高，因为所有的宏代码即用来生成新表达式的代码，都是在编译期运行的。换句话说，编译器将产生完全相同的代码，无论你写成
`(backwards ("hello, world" t format))` 还是 `(format t "hello, world")`。

So how does that help with the code duplication in `where`? Well, you
can write a macro that generates exactly the code you need for each
particular call to `where`. Again, the best approach is to build our
code bottom up. In the hand-optimized selector function, you had an
expression of the following form for each actual field referred to in
the original call to where:

那么这些东西又能对消除 `where`
里的代码重复有什么帮助呢？情况是这样的：可以写出一个宏，它在每个特定的 `where`
调用里只生成真正需要的代码。最佳方法还是自底向上构建我们的代码。在手工优化的选择器函数里，对于每个实际在最初的
`where` 调用中引用的字段。来说，都有一个下列形式的表达式：

```lisp
(equal (getf cd field) value)
```

So let's write a function that, given the name of a field and a value,
returns such an expression. Since an expression is just a list, you
might think you could write something like this:

那么让我们来编写一个给定字段名及值并返回表达式的函数。由于表达式本身只是列表，所以函数应写成下面这样：

```lisp
(defun make-comparison-expr (field value)    ; wrong
  (list equal (list getf cd field) value))
```

However, there's one trick here: as you know, when Lisp sees a simple
name such as `field` or `value` other than as the first element of a list,
it assumes it's the name of a variable and looks up its value. That's
fine for `field` and `value`; it's exactly what you want. But it will
treat `equal`, `getf`, and `cd` the same way, which _isn't_ what you
want. However, you also know how to stop Lisp from evaluating a form:
stick a single forward quote (`'`) in front of it. So if you write
`make-comparison-expr` like this, it will do what you want:

但这里还有一件麻烦事：你知道，当 Lisp 看到一个诸如 `field` 或
`value` 这样的简单名字不作为列表的第一个元素出现时，它会假设这是一个变量的名字并去查找它的值。这对于
`field` 和 `value` 来说是对的，这正是你想要的。但是它也会以同样的方式对待
`equal`、`getf` 以及 `cd`，而这就不是你想要的了。尽管如此，你也知道如何防止
Lisp 去求值一个形式：在它前面加一个单引号。因此如果你将
`make-comparison-expr` 写成下面这样，它将如你所愿：

```lisp
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
```

You can test it out in the REPL.

你可以在 REPL 里测试它。

```lisp
CL-USER> (make-comparison-expr :rating 10)
(EQUAL (GETF CD :RATING) 10)
CL-USER> (make-comparison-expr :title "Give Us a Break")
(EQUAL (GETF CD :TITLE) "Give Us a Break")
```

It turns out that there's an even better way to do it. What you'd
really like is a way to write an expression that's mostly not
evaluated and then have some way to pick out a few expressions that
you _do_ want evaluated. And, of course, there's just such a
mechanism. A back quote (`` ` ``) before an expression stops evaluation just
like a forward quote.

其实还有更好的办法。当你一般不对表达式求值，但又希望通过一些方法从中提取出确实想求值的少数表达式时，你真正需要的是一种书写表达式的方式。但还可以某种提取出其中的你确实想求值的少数表达式来。当然，确实存在这样一种方法。位于表达式之前的反引号、可以像引号那样阻止表达式被求值。

```lisp
CL-USER> `(1 2 3)
(1 2 3)
CL-USER> '(1 2 3)
(1 2 3)
```

However, in a back-quoted expression, any subexpression that's
preceded by a comma is evaluated. Notice the effect of the comma in
the second expression:

不同的是，在一个反引用表达式里，任何以逗号开始的子表达式都是被求值的。请注意下面第二个表达式中逗号的影响。

```lisp
`(1 2 (+ 1 2))        ==> (1 2 (+ 1 2))
`(1 2 ,(+ 1 2))       ==> (1 2 3)
```

Using a back quote, you can write `make-comparison-expr` like this:

有了反引号，就可以像下面这样书写 `make-comparison-expr` 了。

```lisp
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
```

Now if you look back to the hand-optimized selector function, you can
see that the body of the function consisted of one comparison
expression per field/value pair, all wrapped in an **AND**
expression. Assume for the moment that you'll arrange for the
arguments to the where macro to be passed as a single list. You'll
need a function that can take the elements of such a list pairwise and
collect the results of calling `make-comparison-expr` on each pair. To
implement that function, you can dip into the bag of advanced Lisp
tricks and pull out the mighty and powerful **LOOP** macro.

现在如果回过头来看那个手工优化的选择器函数，就可以看到其函数体是由每字段/值对应于一个比较表达式组成的，它们全被封装在一个
**AND** 表达式里。假设现在想让 `where`
宏的所有实参排成一列传递进来，你将需要一个函数，可以从这样的列表中成对提取元素，并收集在每对参数上调用
`make-comparison-expr` 的结果。为了实现这个函数，就需要使用一点儿高级
Lisp 技巧——强有力的 **LOOP** 宏。

```lisp
(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))
```

A full discussion of **LOOP** will have to wait until Chapter 22; for now
just note that this **LOOP** expression does exactly what you need: it
loops while there are elements left in the `fields` list, popping off
two at a time, passing them to `make-comparison-expr`, and collecting
the results to be returned at the end of the loop. The **POP** macro
performs the inverse operation of the **PUSH** macro you used to add
records to `*db*`.

关于 **LOOP** 全面的讨论被放到了第 22 章，目前只需了解这个 **LOOP**
表达式刚好做了你想做的事：当 `fields` 列表有剩余元素时它会保持循环，一次弹出两个元素，将它们传递给
`make-comparison-expr`，然后在循环结束时收集所有返回的结果。**POP**
宏所执行的操作与往 `*db*` 中添加记录时所使用的 **PUSH** 宏的操作。

Now you just need to wrap up the list returned by `make-comparison-list`
in an **AND** and an anonymous function, which you can do in the where
macro itself. Using a back quote to make a template that you fill in
by interpolating the value of `make-comparisons-list`, it's trivial.

现在需要将 `make-comparision-list` 所返回的列表封装在一个 **AND**
和一个匿名函数里，这可以由 `where` 宏本身来实现。使用一个反引号来生成一个模板，然后插入
`make-comparisons-list` 的值，很简单。

```lisp
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
```

This macro uses a variant of `,` (namely, the `,@`) before the call to
`make-comparisons-list`. The `,@` "splices" the value of the following
expression--which must evaluate to a list--into the enclosing
list. You can see the difference between `,` and `,@` in the following two
expressions:

这个宏在 `make-comparisons-list` 调用之前使用了 `,` 的变体 `,@`。这个 `,@`
可以将接下来的表达式（必须求值成一个列表）的值嵌入到其外围的列表里。你可以通过下面两个表达式看出
`,` 和 `,@` 之间的区别：

```lisp
`(and ,(list 1 2 3))   ==> (AND (1 2 3))
`(and ,@(list 1 2 3))  ==> (AND 1 2 3)
```

You can also use `,@` to splice into the middle of a list.

也可以使用 `,@` 在列表的中间插入新元素。

```lisp
`(and ,@(list 1 2 3) 4) ==> (AND 1 2 3 4)
```

The other important feature of the where macro is the use of `&rest` in
the argument list. Like `&key`, `&rest` modifies the way arguments are
parsed. With a `&rest` in its parameter list, a function or macro can
take an arbitrary number of arguments, which are collected into a
single list that becomes the value of the variable whose name follows
the `&rest`. So if you call where like this:

`where` 宏的另一个重要特性是在实参列表中使用 `&rest`。和 `&key`
一样，`&rest` 改变了解析参数的方式。当参数列表里带有 `&rest`
时，一个函数或宏可以接受任意数量的实参，它们将被收集到一个单一列表中，并成为那个跟在
`&rest` 后面的名字所对应的变量的值。因此如果像下面这样调用 `where` 的话。

```lisp
(where :title "Give Us a Break" :ripped t)
```

the variable `clauses` will contain the list.

那么变量 `clauses` 将包含这个列表。

```lisp
(:title "Give Us a Break" :ripped t)
```

This list is passed to `make-comparisons-list`, which returns a list of
comparison expressions. You can see exactly what code a call to where
will generate using the function **MACROEXPAND-1**. If you pass
**MACROEXPAND-1**, a form representing a macro call, it will call the
macro code with appropriate arguments and return the expansion. So you
can check out the previous where call like this:

这个列表被传递给了 `make-comparisons-list`，其返回一个由比较表达式所组成的列表。可以通过使用函数
**MACROEXPAND-1** 来精确地看到一个 `where`
调用将产生出哪些代码。如果传给 **MACROEXPAND-1**
一个代表宏调用的形式，它将使用适当的参数来调用宏代码并返回其展开式。因此可以像这样检查上一个
`where`调用：

```lisp
CL-USER> (macroexpand-1 '(where :title "Give Us a Break" :ripped t))
#'(LAMBDA (CD)
    (AND (EQUAL (GETF CD :TITLE) "Give Us a Break")
         (EQUAL (GETF CD :RIPPED) T)))
T
```

Looks good. Let's try it for real.

看起来不错。现在让我们实际试一下。

```lisp
CL-USER> (select (where :title "Give Us a Break" :ripped t))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
```

It works. And the where macro with its two helper functions is
actually one line shorter than the old where function. And it's more
general in that it's no longer tied to the specific fields in our CD
records.

它成功了。并且事实上，新的 `where` 宏加上它的两个助手函数还比老的 `where`
函数少了一行代码。并且新的代码更加通用，再也不需要理会我们 CD
记录中的特定字段了。
