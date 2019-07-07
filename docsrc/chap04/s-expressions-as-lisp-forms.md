# S-expressions As Lisp Forms（作为 Lisp 形式的 S-表达式）

After the reader has translated a bunch of text into s-expressions,
the s-expressions can then be evaluated as Lisp code. Or some of them
can--not every s-expressions that the reader can read can necessarily
be evaluated as Lisp code. Common Lisp's evaluation rule defines a
second level of syntax that determines which s-expressions can be
treated as Lisp forms. The syntactic rules at this level are quite
simple. Any atom--any nonlist or the empty list--is a legal Lisp form
as is any list that has a symbol as its first element.

在读取器把大量文本转化为 S-表达式以后，这些 S-表达式随后可以作为 Lisp
代码被求值。或者只有它们中的一些可以——并不是每个读取器可读的
S-表达式都有必要作为 Lisp 表达式来求值的，Common Lisp
的求值规则定义了第二层的语法来检测哪种 S-表达式可看作 Lisp
形式。这一层面的句法规则相当简单。任何原子（非列表或空列表）都是一个合法的
Lisp 形式，正如任何以符号为首元素的列表那样。

Of course, the interesting thing about Lisp forms isn't their syntax
but how they're evaluated. For purposes of discussion, you can think
of the evaluator as a function that takes as an argument a
syntactically well-formed Lisp form and returns a value, which we can
call the value of the form. Of course, when the evaluator is a
compiler, this is a bit of a simplification--in that case, the
evaluator is given an expression and generates code that will compute
the appropriate value when it's run. But this simplification lets me
describe the semantics of Common Lisp in terms of how the different
kinds of Lisp forms are evaluated by this notional function.

当然，Lisp 形式的有趣之处不在于其语法而在于其被求值的方式。为了方便讨论，你可以将求值器想象成一个函数，它接受一个句法良好定义的
Lisp 形式作为参数并返回一个值，我们称之为这个形式的值。当然，当求值器是一个编译器时，情况会更加简化一些——在那种情况下，求值器被给定一个表达式，然后生成在其运行时可以计算出相应值的代码。但是这种简化可以让我们从不同类型的
Lisp 形式如何被这个假想的函数求值的角度来描述 Common Lisp 的语义。

The simplest Lisp forms, atoms, can be divided into two categories:
symbols and everything else. A symbol, evaluated as a form, is
considered the name of a variable and evaluates to the current value
of the variable. I'll discuss in Chapter 6 how variables get their
values in the first place. You should also note that certain
"variables" are that old oxymoron of programming: "constant
variables." For instance, the symbol `PI` names a constant variable
whose value is the best possible floating-point approximation to the
mathematical constant π.

作为最简单的 Lisp
形式，原子可以被分成两个类别：符号和所有其他内容。符号在作为形式被求值时会被视为一个变量名，并且会被求值为该变量的当前值。第 6 章将讨论变量是如何得到这个值的。你也需要注意，某些特定的 “变量” 其实是编程领域的早期产物
“常值变量”（constant
variable）。例如，符号 **PI** 命名了一个常值变量，其值是最接近数学常量 π 的浮点数。

All other atoms--numbers and strings are the kinds you've seen so
far--are self-evaluating objects. This means when such an expression
is passed to the notional evaluation function, it's simply
returned. You saw examples of self-evaluating objects in Chapter 2
when you typed 10 and "hello, world" at the REPL.

所有其他的原子，包括你已经见过的数字和字符串，都是自求值对象。这意味着当这样的表达式被传递给假想函数时，它会简单地直接返回自身。第2章在 REPL
里键入的 10 和 "hello, world" 实际上就是自求值对象的例子。

It's also possible for symbols to be self-evaluating in the sense that
the variables they name can be assigned the value of the symbol
itself. Two important constants that are defined this way are **T** and
**NIL**, the canonical true and false values. I'll discuss their role as
booleans in the section "Truth, Falsehood, and Equality."

把符号变成自求值对象也是可能的——它们所命名的变量可以被赋值成符号本身的值。两个以这种方式定义的常量是
**T** 和 **NIL**，即所谓的真值和假值。我将在 “真、假和等价”
那一节里讨论它们作为布尔值的角色。

Another class of self-evaluating symbols are the keyword
symbols--symbols whose names start with :. When the reader interns
such a name, it automatically defines a constant variable with the
name and with the symbol as the value.

另一类自求值符号是关键字符号——名字以冒号开始的符号。当读取器保留这样一个名字时，它会自动定义一个以此命名的常值变量并以该符号作为其值。

Things get more interesting when we consider how lists are
evaluated. All legal list forms start with a symbol, but three kinds
of list forms are evaluated in three quite different ways. To
determine what kind of form a given list is, the evaluator must
determine whether the symbol that starts the list is the name of a
function, a macro, or a special operator. If the symbol hasn't been
defined yet--as may be the case if you're compiling code that contains
references to functions that will be defined later--it's assumed to be
a function name. I'll refer to the three kinds of forms as function
call forms, macro forms, and special forms.

当我们开始考虑列表的求值方式时，事情变得更加有趣了。所有合法的列表形式均以一个符号开始，但是有三种类型的列表形式，它们会以三种相当不同的方式进行求值。为了确定一个给定的列表是哪种形式，求值器必须检测列表开始处的那个符号是一个函数、宏还是特殊操作符的名字。如果该符号尚未定义，比如说当你正在编译一段含有对尚未定义函数的引用的代码时，它会被假设成一个函数的名字。 我将把这三种类型的形式称为函数调用形式（function
call form）、宏形式（macro form）和特殊形式（special form）。
