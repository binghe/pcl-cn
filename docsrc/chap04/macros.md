# Macros（宏）

While special operators extend the syntax of Common Lisp beyond what
can be expressed with just function calls, the set of special
operators is fixed by the language standard. Macros, on the other
hand, give users of the language a way to extend its syntax. As you
saw in Chapter 3, a macro is a function that takes s-expressions as
arguments and returns a Lisp form that's then evaluated in place of
the macro form. The evaluation of a macro form proceeds in two phases:
First, the elements of the macro form are passed, unevaluated, to the
macro function. Second, the form returned by the macro
function--called its _expansion_--is evaluated according to the normal
evaluation rules.

虽然特殊操作符以超越了函数调用所能表达的方式扩展了 Common Lisp
语法，但特殊操作符的数量在语言标准中是固定的。然而，宏却能提供给语言用户一种语法扩展方式。如同在第
3 章里看到的那样，宏是一个以 S-表达式为其参数的函数，并返回一个
Lisp 形式，然后对其求值并用该值取代宏形式。宏形式的求值过程包括两个阶段：首先，宏形式的元素不经求值却被传递到宏函数里；其次，由宏函数所返回的形式（称为其_展开式_）按照正常的求值规则进行求值。

It's important to keep the two phases of evaluating a macro form clear
in your mind. It's easy to lose track when you're typing expressions
at the REPL because the two phases happen one after another and the
value of the second phase is immediately returned. But when Lisp code
is compiled, the two phases happen at completely different times, so
it's important to keep clear what's happening when. For instance, when
you compile a whole file of source code with the function
**COMPILE-FILE**, all the macro forms in the file are recursively expanded
until the code consists of nothing but function call forms and special
forms. This macroless code is then compiled into a FASL file that the
**LOAD** function knows how to load. The compiled code, however, isn't
executed until the file is loaded. Because macros generate their
expansion at compile time, they can do relatively large amounts of
work generating their expansion without having to pay for it when the
file is loaded or the functions defined in the file are called.

重点在于要清醒地认识到一个宏形式求值的两个阶段。当你在 REPL
中输入表达式时很容易忘记这一点，因为两个阶段相继发生并且后一阶段的值被立即返回了。但是当
Lisp 代码被编译时，这两个阶段所发生时间却是完全不同的，因此关键在于对何时发生什么要保持清醒。例如，当使用函数
**COMPILE-FILE** 来编译整个源代码文件时，文件中所有宏形式将被递归展开，直到代码中只含有函数调用形式和特殊形式。这些无宏的代码随后被编译成一个
FASL文件——**LOAD**
函数知道如何去加载它。但编译后的代码直到文件被加载时才会被执行。因为宏在编译期会生成其展开式，它们可以用相对大量的工作来生成其展开式，而无需在文件被加载时或是当文件中定义的函数被调用时再付出额外的代价。

Since the evaluator doesn't evaluate the elements of the macro form
before passing them to the macro function, they don't need to be
well-formed Lisp forms. Each macro assigns a meaning to the
s-expressions in the macro form by virtue of how it uses them to
generate its expansion. In other words, each macro defines its own
local syntax. For instance, the `backwards` macro from Chapter 3 defines
a syntax in which an expression is a legal `backwards` form if it's a
list that's the reverse of a legal Lisp form.

由于求值器在将宏形式传递给宏函数之前并不对它们求值，因此它们不需要是格式良好的
Lisp 形式。每个宏都为其宏形式中的符号表达式指定了一种含义，用以指明宏将如何使用它们生成展开式。换句话说，每个宏都定义了它们自己的局部语法。例如，第 3 章的 `backwards` 宏就定义了一种语法，合法的
`backwards` 形式列表必须与合法的 Lisp 形式列表反序。

I'll talk quite a bit more about macros throughout this book. For now
the important thing for you to realize is that macros--while
syntactically similar to function calls--serve quite a different
purpose, providing a hook into the compiler.

我会在本书中经常地提到宏。眼下最为重要的是认识到宏：虽然跟函数调用在句法上相似，但却有着用于相当不同的用途，并提供了一种嵌入编译器的钩子。
