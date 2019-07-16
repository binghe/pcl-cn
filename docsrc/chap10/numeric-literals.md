# Numeric Literals（字面数值）

You can write numeric literals in a variety of ways; you saw a few
examples in Chapter 4. However, it's important to keep in mind the
division of labor between the Lisp reader and the Lisp evaluator--the
reader is responsible for translating text into Lisp objects, and the
Lisp evaluator then deals only with those objects. For a given number
of a given type, there can be many different textual representations,
all of which will be translated to the same object representation by
the Lisp reader. For instance, you can write the integer 10 as `10`,
`20/2`, `#xA`, or any of a number of other ways, but the reader will
translate all these to the same object. When numbers are printed back
out--say, at the REPL--they're printed in a canonical textual syntax
that may be different from the syntax used to enter the number. For
example:

可以用多种方式来书写字面数值，第4章已经介绍了一些例子。但你需要牢记
Lisp 读取器和 Lisp 求值器之间的分工——读取器负责将文本转化成
Lisp 对象，而 Lisp
求值器只处理这些对象。对于一个给定类型的给定数字来说，它可以有多种不同的字面表示方式，所有这些都将被
Lisp 读取器转化成相同的对象表示。例如，你可以将整数 10
写成 `10`、`20/2`、`#xA`
或是其他形式的任何数字，但读取器将把所有这些转化成同一个对象。当数字被打印回来时，比如说在
REPL 中，它们将以一种可能与输入该数字时不同的规范化文本语法被打印出来。例如：

```lisp
CL-USER> 10
10
CL-USER> 20/2
10
CL-USER> #xa
10
```

The syntax for integer values is an optional sign (`+` or `-`) followed by
one or more digits. Ratios are written as an optional sign and a
sequence of digits, representing the numerator, a slash (`/`), and
another sequence of digits representing the denominator. All rational
numbers are "canonicalized" as they're read--that's why `10` and `20/2`
are both read as the same number, as are `3/4` and `6/8`. Rationals are
printed in "reduced" form--integer values are printed in integer
syntax and ratios with the numerator and denominator reduced to lowest
terms.

整数的语法是可选的符号（`+` 或
`-`）后接一个或多个数字。比值的写法则依次组合了一个可选符号、一个代表分子的数位序列、一个斜杠（`/`）以及另一个代表分母的数位序列。所有的有理数在读取后都被
“规范化”，这就是 `10` 和 `20/2` 都被读成同一个数字的原因，`3/4` 和
`6/8` 也是这样，有理数以 “简化”
形式打印，整数值以整数语法来打印，而比值被打印成分值和分母约分到最简的形式。

It's also possible to write rationals in bases other than 10. If
preceded by `#B` or `#b`, a rational literal is read as a binary number
with 0 and 1 as the only legal digits. An `#O` or `#o` indicates an octal
number (legal digits `0-7`), and `#X` or `#x` indicates hexadecimal (legal
digits `0-F` or `0-f`). You can write rationals in other bases from 2 to
36 with `#nR` where `n` is the base (always written in
decimal). Additional "digits" beyond 9 are taken from the letters `A-Z`
or `a-z`. Note that these radix indicators apply to the whole
rational--it's not possible to write a ratio with the numerator in one
base and denominator in another. Also, you can write integer values,
but not ratios, as decimal digits terminated with a decimal point.6
Some examples of rationals, with their canonical, decimal
representation are as follows:

用 10 以外的进制来书写有理数也是可能的。如果前缀 `#B` 或
`#b`，一个字面有理数将被作为二进制读取，其中 0 和 1
是唯一的合法数字。前缀 `#O` 或 `#o` 代表一个八进制数（合法数字
`0-7`），而 `#X` 或 `#x` 则代表十六进制数（合法数字 `0-F` 或
`o-f`）。你可以使用 `#nR` 以 2 到 36 的其他进制书写有理数，其中
`n` 代表进制数（一定要以十进制书写）。超过 9 的附加 “数字” 从字母
`A-Z` 或 `a-z` 中获取。注意，这些进制指示符将应用到整个有理数上——不可能以一种进制来书写比值的分值，而用另一种进制来书写分母。另外，你可以将整数而非比值写成以十进制小数点结尾的十进制数。下面是一些有理数的例子，带有它们对应的规范化十进制表示：

```lisp
123                            ==> 123
+123                           ==> 123
-123                           ==> -123
123.                           ==> 123
2/3                            ==> 2/3
-2/3                           ==> -2/3
4/6                            ==> 2/3
6/3                            ==> 2
#b10101                        ==> 21
#b1010/1011                    ==> 10/11
#o777                          ==> 511
#xDADA                         ==> 56026
#36rABCDEFGHIJKLMNOPQRSTUVWXYZ ==> 8337503854730415241050377135811259267835
```

You can also write floating-point numbers in a variety of ways. Unlike
rational numbers, the syntax used to notate a floating-point number
can affect the actual type of number read. Common Lisp defines four
subtypes of floating-point number: short, single, double, and
long. Each subtype can use a different number of bits in its
representation, which means each subtype can represent values spanning
a different range and with different precision. More bits gives a
wider range and more precision.

也可以用多种方式来书写浮点数。和有理数不同，表示浮点数的语法可以影响数字被读取的实际类型。Common
Lisp
定义了四种浮点数子类型：短型、单精度、双精度和长型。每一个子类型在其表示中可以使用不同数量的比特，这意味着每个子类型可以表示跨越不同范围和精度的值。更多的比特可以获得更宽的范围和更高的精度。

The basic format for floating-point numbers is an optional sign
followed by a nonempty sequence of decimal digits possibly with an
embedded decimal point. This sequence can be followed by an exponent
marker for "computerized scientific notation." The exponent marker
consists of a single letter followed by an optional sign and a
sequence of digits, which are interpreted as the power of ten by which
the number before the exponent marker should be multiplied. The letter
does double duty: it marks the beginning of the exponent and indicates
what floating- point representation should be used for the number. The
exponent markers s, f, d, l (and their uppercase equivalents) indicate
short, single, double, and long floats, respectively. The letter e
indicates that the default representation (initially single-float)
should be used.

浮点数的基本格式是一个可选符号后跟一个非空的十进制数字序列，同时可能带有一个嵌入的小数点。这个序列可能后接一个代表
“计算机科学计数法”
的指数标记。指数标记由单个字母后跟一个可选符号和一个数字序列组成，其代表
10
的指数用来跟指数标记前的数字相乘。该字母有两重作用：它标记了指数的开始并且指出该数字应当使用的浮点表示方式。指数标记
s、f、d、l（以及它们等价的大写形式）分别代表短型、单精度、双精度以及长型浮点数。字母
e 代表默认表示方式（单浮点数）。

Numbers with no exponent marker are read in the default representation
and must contain a decimal point followed by at least one digit to
distinguish them from integers. The digits in a floating-point number
are always treated as base 10 digits--the `#B`, `#X`, `#O`, and `#R` syntaxes
work only with rationals. The following are some example
floating-point numbers along with their canonical representation:

没有指数标记的数字以默认表示来读取，并且必须含有一个小数点后面还至少有一个数字，以区别于整数。浮点数中的数字总是以十进制数字来表示——`#B`、`#X`、`#O`
和 `#R` 语法只用在有理数上。下面是一些浮点数的例子，带有它们的规范表示形式：

```lisp
1.0      ==> 1.0
1e0      ==> 1.0
1d0      ==> 1.0d0
123.0    ==> 123.0
123e0    ==> 123.0
0.123    ==> 0.123
.123     ==> 0.123
123e-3   ==> 0.123
123E-3   ==> 0.123
0.123e20 ==> 1.23e+19
123d23   ==> 1.23d+25
```

Finally, complex numbers are written in their own syntax, namely, `#C`
or `#c` followed by a list of two real numbers representing the real and
imaginary part of the complex number. There are actually five kinds of
complex numbers because the real and imaginary parts must either both
be rational or both be the same kind of floating-point number.

最后，复数有它们自己的语法，也就是 `#C` 或 `#c`
跟上一个由两个实数所组成的列表，分别代表实数的实部和虚部。事实上因为实部和虚部必须同为有理数或相同类型的浮点数，所以共有五种类型的复数。

But you can write them however you want--if a complex is written with
one rational and one floating-point part, the rational is converted to
a float of the appropriate representation. Similarly, if the real and
imaginary parts are both floats of different representations, the one
in the smaller representation will be *upgraded*.

不过你可以随意书写它们。如果复数被写成由有理数和浮点数组成，该有理数将被转化成一个适当表示的浮点数。类似地，如果实部和虚部是不同表示法的浮点数，使用较小表示法的那一个将被升级。

However, no complex numbers have a rational real component and a zero
imaginary part--since such values are, mathematically speaking,
rational, they're represented by the appropriate rational value. The
same mathematical argument could be made for complex numbers with
floating-point components, but for those complex types a number with a
zero imaginary part is always a different object than the
floating-point number representing the real component. Here are some
examples of numbers written the complex number syntax:

尽管如此，没有复数可以具有一个有理的实部和一个零的虚部，因为这样的值从数学上讲是有理的，所以它们将用对应的有理数值来表示。同样的数学论据对于由浮点数所组成的复数也是成立的，但其中那些带有零虚部的复数总是一个与代表实部的浮点数不同的对象。下面是一些以复数语法写成的数字的例子：

```lisp
#c(2      1)    ==> #c(2 1)
#c(2/3  3/4)    ==> #c(2/3 3/4)
#c(2    1.0)    ==> #c(2.0 1.0)
#c(2.0  1.0d0)  ==> #c(2.0d0 1.0d0)
#c(1/2  1.0)    ==> #c(0.5 1.0)
#c(3      0)    ==> 3
#c(3.0  0.0)    ==> #c(3.0 0.0)
#c(1/2    0)    ==> 1/2
#c(-6/3   0)    ==> -2
```
