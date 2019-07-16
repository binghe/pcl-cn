# Numbers（数值）

Math, as Barbie says, is hard. Common Lisp can't make the math part
any easier, but it does tend to get in the way a lot less than other
programming languages. That's not surprising given its mathematical
heritage. Lisp was originally designed by a mathematician as a tool
for studying mathematical functions. And one of the main projects of
the MAC project at MIT was the Macsyma symbolic algebra system,
written in Maclisp, one of Common Lisp's immediate
predecessors. Additionally, Lisp has been used as a teaching language
at places such as MIT where even the computer science professors
cringe at the thought of telling their students that 10/4 = 2, leading
to Lisp's support for exact ratios. And at various times Lisp has been
called upon to compete with FORTRAN in the high-performance numeric
computing arena.

正如 Barbie 所说，数学很难。虽说 Common Lisp
并不能使其数学部分变得简单一些，但它确实可以比其它编程语言在这方面简单不少，考虑到它的数学传统这并不奇怪。Lisp
最初是用作研究数学函数的工具由一位数学家设计而成的。并且
MIT 的 MAC 项目的主要成果——Macsyma 符号代数系统也是由
Maclisp（一种 Common Lisp 的前身）写成的。此外，Lisp 还曾被用作 MIT
这类院校的教学语言，在那里即便连计算机学教授们也不愿意告诉学生 10/4=2，从而使
Lisp 能够支持精确比值。Lisp 还曾经多次在高性能数值计算领域与 FORTRAN 竞争。

One of the reasons Lisp is a nice language for math is its numbers
behave more like true mathematical numbers than the approximations of
numbers that are easy to implement in finite computer hardware. For
instance, integers in Common Lisp can be almost arbitrarily large
rather than being limited by the size of a machine word.3 And dividing
two integers results in an exact ratio, not a truncated value. And
since ratios are represented as pairs of arbitrarily sized integers,
ratios can represent arbitrarily precise fractions.

Lisp
是一门用于数学的良好语言，其原因之一是它的数字更加接近于真正的数学数字而不是易于在有穷计算机硬件上实现的近似数字。例如，Common Lisp
中的整数可以几乎是任意大而不是被限制在一个机器字的大小上。而两个整数相除将得到一个确切的比值而非截断的值。并且比值是由成对的任意大小的整数表示的，所以比值可以表示任意精度的分数。

On the other hand, for high-performance numeric programming, you may
be willing to trade the exactitude of rationals for the speed offered
by using the hardware's underlying floating-point operations. So,
Common Lisp also offers several types of floating-point numbers, which
are mapped by the implementation to the appropriate hardware-supported
floating-point representations.5 Floats are also used to represent the
results of a computation whose true mathematical value would be an
irrational number.

另一方面，对于高性能数值编程，你可能想要用有理数的精度来换取使用硬件的底层浮点操作所得到的速度。因此
Common Lisp
也提供了几种浮点数，它们可以映射到适当的硬件支持浮点表达的实现上。浮点数也被用于表示其真正数学值为无理数的计算结果。

Finally, Common Lisp supports complex numbers--the numbers that result
from doing things such as taking square roots and logarithms of
negative numbers. The Common Lisp standard even goes so far as to
specify the principal values and branch cuts for irrational and
transcendental functions on the complex domain.

最后，Common Lisp 支持复数——通过在负数上获取平方根和对数所得到的结果。Common
Lisp 标准甚至还指定了复域上无理和超越函数的主值和分支截断。
