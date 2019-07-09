# **WHEN** and **UNLESS**（**WHEN** 和 **UNLESS**）

As you've already seen, the most basic form of conditional
execution--if `x`, do `y`; otherwise do `z`--is provided by the **IF** special
operator, which has this basic form:

如前所述，最基本的条件执行形式是由 **IF**
特殊操作符提供的，其基本形式是：如果 `x` 成立，那么执行 `y`；否则执行 `z`。

```lisp
(if condition then-form [else-form])
```

The `condition` is evaluated and, if its value is non-**NIL**, the `then-form`
is evaluated and the resulting value returned. Otherwise, the
`else-form`, if any, is evaluated and its value returned. If `condition`
is **NIL** and there's no `else-form`, then the **IF** returns **NIL**.

`condition` 被求值，如果其值非 **NIL**，那么 `then-form`
会被求值并返回其结果。否则，如果有 `else-form`
的话，它将被求值并返回其结果。如果 `condition`是 **NIL**
并且没有 `else-form`，那么IF返回 **NIL**。

```lisp
(if (> 2 3) "Yup" "Nope") ==> "Nope"
(if (> 2 3) "Yup")        ==> NIL
(if (> 3 2) "Yup" "Nope") ==> "Yup"
```

However, **IF** isn't actually such a great syntactic construct because
the then-form and else-form are each restricted to being a single Lisp
form. This means if you want to perform a sequence of actions in
either clause, you need to wrap them in some other syntax. For
instance, suppose in the middle of a spam-filtering program you wanted
to both file a message as spam and update the spam database when a
message is spam. You can't write this:

尽管如此，**IF** 事实上并不是什么伟大的句法构造，因为每个 `then-form`
和 `else-form` 都被限制在必须是单一的 Lisp
形式上。这意味着如果想在每个子句中执行一系列操作，则必须将其用其他一些语法形式进行封装。举个例子，假如在一个垃圾过滤程序中，当一个消息是垃圾时，你想要在将其标记为垃圾的同时更新垃圾数据库，那么你不能这样写：

```lisp
(if (spam-p current-message)
    (file-in-spam-folder current-message)
    (update-spam-database current-message))
```

because the call to `update-spam-database` will be treated as the else
clause, not as part of the then clause. Another special operator,
**PROGN**, executes any number of forms in order and returns the value of
the last form. So you could get the desired behavior by writing the
following:

因为对 `update-spam-database` 的调用将被作为 else
子句来看待，而不是 then
子句的一部分。另一个特殊操作符 **PROGN**
可以按顺序执行任意数量的形式并返回最后一个形式的值。因此可以通过写成下面这样来得到预想的行为：

```lisp
(if (spam-p current-message)
    (progn
      (file-in-spam-folder current-message)
      (update-spam-database current-message)))
```

That's not too horrible. But given the number of times you'll likely
have to use this idiom, it's not hard to imagine that you'd get tired
of it after a while. "Why," you might ask yourself, "doesn't Lisp
provide a way to say what I really want, namely, 'When x is true, do
this, that, and the other thing'?" In other words, after a while you'd
notice the pattern of an **IF** plus a **PROGN** and wish for a way to
abstract away the details rather than writing them out every time.

这样做并不算太坏。但假如不得不多次使用这样的写法，不难想象你将在一段时间以后开始厌倦它。你可能会自问：“为什么
Lisp 没有提供一种方式来做我真正想做的事，也就是说，‘当 `x`
为真时，做这个、那个以及其他一些事情’？”
换句话说，很快你将注意到这种由 **IF** 加上 **PROGN**
所组成的模式，并且希望可以有一种方式来抽象掉所有细节而不是每次都将它们写出来。

This is exactly what macros provide. In this case, Common Lisp comes
with a standard macro, **WHEN**, which lets you write this:

这正是宏所能够提供的功能。在这个案例中，Common Lisp
提供了一个标准宏 **WHEN**，可以让你写成这样：

```lisp
(when (spam-p current-message)
  (file-in-spam-folder current-message)
  (update-spam-database current-message))
```

But if it wasn't built into the standard library, you could define
**WHEN** yourself with a macro such as this, using the backquote
notation I discussed in Chapter 3:
  
但如果它没有被内置到标准库中，你也可以像下面这样用一个宏来自己定义
**WHEN**，这里用到了第 3 章中讨论过的反引号：

```lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```

A counterpart to the **WHEN** macro is **UNLESS**, which reverses the
condition, evaluating its body forms only if the condition is
false. In other words:

与 **WHEN** 宏同系列的另一个宏是
**UNLESS**，它取相反的条件，只有当条件为假时才求值其形式体。换句话说：

```lisp
(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))
```

Admittedly, these are pretty trivial macros. There's no deep black
magic here; they just abstract away a few language-level bookkeeping
details, allowing you to express your true intent a bit more
clearly. But their very triviality makes an important point: because
the macro system is built right into the language, you can write
trivial macros like **WHEN** and **UNLESS** that give you small but real
gains in clarity that are then multiplied by the thousands of times
you use them. In Chapters 24, 26, and 31 you'll see how macros can
also be used on a larger scale, creating whole domain-specific
embedded languages. But first let's finish our discussion of the
standard control-construct macros.
  
必须承认，这些都是相当简单的宏。这里没有什么高深的道理，它们只是抽象掉了一些语言层面约定俗成的细节，从而允许你更加清晰地表达你的真实意图。但是它们的极度简单性却产生了一个重要的观点：由于宏系统是直接构建在语言之中的，所以可以写出像
**WHEN** 和 **UNLESS**
这样简单的宏来获得虽小但却重要的清晰性，并随后通过不断地使用而无限放大。第
24、26 和 31 章将展现宏是如何被更大规模地用于创建完整的特定领域的嵌入式语言。但首先来介绍一下标准控制构造宏。
