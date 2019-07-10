# COND（COND 宏）

Another time raw **IF** expressions can get ugly is when you have a
multibranch conditional: if `a` do x, else if `b` do y; else do z. There's
no logical problem writing such a chain of conditional expressions
with just **IF**, but it's not pretty.

当遇到多重分支的条件语句时，原始的 **IF**
表达式再一次变得丑陋不堪：如果 `a` 成立那么执行 x，否则如果 `b`
成立那么执行 y；否则执行 z。只用 **IF**
来写这样的条件表达式链并没有逻辑问题，只是不太好看。

```lisp
(if a
    (do-x)
    (if b
       (do-y)
       (do-z)))
```

And it would be even worse if you needed to include multiple forms in
the then clauses, requiring **PROGN**s. So, not surprisingly, Common Lisp
provides a macro for expressing multibranch conditionals: **COND**. This
is the basic skeleton:

并且如果需要在 then 子句中包括多个形式，那就需要用到
**PROGN**，而那样事情就会变得更糟。因此毫不奇怪地，Common Lisp
提供了一个用于表达多重分支条件的宏 **COND**。下面是它的基本结构：

```lisp
(cond
  (test-1 form*)
      .
      .
      .
  (test-N form*))
```

Each element of the body represents one branch of the conditional and
consists of a list containing a condition form and zero or more forms
to be evaluated if that branch is chosen. The conditions are evaluated
in the order the branches appear in the body until one of them
evaluates to true. At that point, the remaining forms in that branch
are evaluated, and the value of the last form in the branch is
returned as the value of the **COND** as a whole. If the branch contains
no forms after the condition, the value of the condition is returned
instead. By convention, the branch representing the final else clause
in an if/else-if chain is written with a condition of **T**. Any non-**NIL**
value will work, but a **T** serves as a useful landmark when reading the
code. Thus, you can write the previous nested IF expression using **COND**
like this:

主体中的每个元素都代表一个条件分支，并由一个列表所构成，列表中含有一个条件形式，以及零或多个当该分支被选择时将被求值的形式。这些条件形式按照分支在主体中出现的顺序被依次求值，直到它们中的一个求值为真。这时，该分支中的其余形式将被求值，且分支中最后一个形式的值将被作为整个
**COND** 的返回值。如果该分支在条件形式之后不再含有其他形式，那么就将
返回该条件形式的值。习惯上，那个用来表示
if/else-if 链中最后一个 else
子句的分支将被写成带有条件 **T**。虽然任何非 **NIL**
的值都可以使用，但在阅读代码时，**T**
标记确实有用。这样就可以像下面这样用 **COND**
来写出前面的嵌套 **IF** 表达式：

```lisp
(cond (a (do-x))
      (b (do-y))
      (t (do-z)))
```
