# Variables（变量）

The next basic building block we need to look at are variables. Common
Lisp supports two kinds of variables: lexical and dynamic.1 These two
types correspond roughly to "local" and "global" variables in other
languages. However, the correspondence is only approximate. On one
hand, some languages' "local" variables are in fact much like Common
Lisp's dynamic variables.2 And on the other, some languages' local
variables are lexically scoped without providing all the capabilities
provided by Common Lisp's lexical variables. In particular, not all
languages that provide lexically scoped variables support closures.

我们需要了解的下一个基本程序构造单元是变量。Common Lisp
支持两种类型的变量：词法（lexical）变量和动态（dynamic）变量。这两种变
量类型分别对应于其他语言中的局部变量和全局变量，不过也只能说是大致相似。一方面，某些语言中的局部变量更像是
Common Lisp
的动态变量 。另一方面，某些语言中的局部变量虽然是词法作用域的，但却并没有提供由
Common Lisp
的词法变量所提供的所有功能。尤其是并非所有语言都提供了支持闭包的词法作用域变量。

To make matters a bit more confusing, many of the forms that deal with
variables can be used with both lexical and dynamic variables. So I'll
start by discussing a few aspects of Lisp's variables that apply to
both kinds and then cover the specific characteristics of lexical and
dynamic variables. Then I'll discuss Common Lisp's general-purpose
assignment operator, SETF, which is used to assign new values to
variables and just about every other place that can hold a value.

许多含有变量的表达式都可以同时使用词法和动态变量，这样一来更令人困惑了。因此本章我先讨论同时涉及到两种类型的
Lisp 变量的几个方面，然后再谈及词法变量和动态变量各自的特征。随后再讨论
Common Lisp
的通用赋值操作符 **SETF**，它用于为变量和其他任何可以保存值的地方赋予新值。
