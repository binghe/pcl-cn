# DEFMACRO（DEFMACRO 宏）

As you saw in Chapter 3, macros really are defined with **DEFMACRO**
forms, though it stands--of course--for *DEFine MACRO*, not Definition
for Mac. The basic skeleton of a **DEFMACRO** is quite similar to the
skeleton of a **DEFUN**.

如同你在第 3 章里所看到的那样，宏真的是用 **DEFMACRO**
来定义的。当然，它代表的是 “定义宏” (*DEFine MACRO*)而不是
“给 Mac 的定义”（Definition for Mac）。**DEFMACRO**
的基本框架和 **DEFUN** 框架很相似。

```lisp
(defmacro name (parameter*)
  "Optional documentation string."
  body-form*)
```

Like a function, a macro consists of a name, a parameter list, an
optional documentation string, and a body of Lisp expressions.
However, as I just discussed, the job of a macro isn't to do anything
directly--its job is to generate code that will later do what you
want.

和函数一样，宏由名字、形参列表、可选文档字符串以及
Lisp 表达式体所构成。但如前所述，宏并不直接做任何事，它只是用于生成以后工作所需的代码。

Macros can use the full power of Lisp to generate their expansion,
which means in this chapter I can only scratch the surface of what you
can do with macros. I can, however, describe a general process for
writing macros that works for all macros from the simplest to the most
complex.

宏可以使用 Lisp
的所有功能来生成其展开式，这意味着本章只能初步说明宏的具体功用。不过我却可以描述一个通用的宏编写过程，它适用于从最简单到最复杂的所有宏。

The job of a macro is to translate a macro form--in other words, a
Lisp form whose first element is the name of the macro--into code that
does a particular thing. Sometimes you write a macro starting with the
code you'd like to be able to write, that is, with an example macro
form. Other times you decide to write a macro after you've written the
same pattern of code several times and realize you can make your code
clearer by abstracting the pattern.

宏的工作是将宏形式，首元素为宏名的 Lisp
形式，转化成做特定事情的代码。有时是从想要编写的代码开始来编写宏的，就是说从一个示例的宏形式开始。其他时候则是在连续几次编写了相同的代码模式并认识到通过抽象该模式可以使代码更清晰后，才开始决定编写宏的。

Regardless of which end you start from, you need to figure out the
other end before you can start writing a macro: you need to know both
where you're coming from and where you're going before you can hope to
write code to do it automatically. Thus, the first step of writing a
macro is to write at least one example of a call to the macro and the
code into which that call should expand.

无论从哪一端开始，你都需要在开始编写宏之前搞清楚另一端：既需要知道从哪里开始，又要知道正在向何处去，然后才能期待编写代码来自动地做到这点。因此编写宏的第一步是至少应去编写一个宏调用的示例以及该调用应当展开成的代码。

Once you have an example call and the desired expansion, you're ready
for the second step: writing the actual macro code. For simple macros
this will be a trivial matter of writing a backquoted template with
the macro parameters plugged into the right places. Complex macros
will be significant programs in their own right, complete with helper
functions and data structures.

一旦有了示例调用及预想的展开式，那么就可以开始第二步了：编写实际的宏代码。对于简单的宏来说，这将极其轻松——编写一个反引用模板并将宏参数插入到正确的位置上。复杂的宏则会是一个庞大的独立程序，它将带有配套的助手函数和数据结构。

After you've written code to translate the example call to the
appropriate expansion, you need to make sure the abstraction the macro
provides doesn't "leak" details of its implementation. Leaky macro
abstractions will work fine for certain arguments but not others or
will interact with code in the calling environment in undesirable
ways. As it turns out, macros can leak in a small handful of ways, all
of which are easily avoided as long as you know to check for
them. I'll discuss how in the section "Plugging the Leaks."

在已经编写了代码来完成从示例调用到适当的展开式的转换以后，需要确保宏所提供的抽象没有
“泄漏” 其实现细节。有漏洞的宏抽象将只适用于特定参数上，或会以预想之外的方式与调用环境中的代码进行交互。后面将会看到，宏只能以很少的几种方式泄漏，而所有这些都是可以轻易避免的，只要知道如何检查它们就行。第
8.7 节将讨论具体的方法。

To sum up, the steps to writing a macro are as follows:

1. Write a sample call to the macro and the code it should expand into, or vice versa.
2. Write code that generates the handwritten expansion from the arguments in the sample call.
3. Make sure the macro abstraction doesn't "leak."

总结起来，编写宏的步骤如下所示：

1. 编写示例的宏调用以及它应当展开成的代码，反之亦然。
2. 编写从示例调用的参数中生成手写展开式的代码。
3. 确保宏抽象不产生 “泄漏”。
