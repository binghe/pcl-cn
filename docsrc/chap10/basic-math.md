# Basic Math（初等数学）

The basic arithmetic operations--addition, subtraction,
multiplication, and division--are supported for all the different
kinds of Lisp numbers with the functions `+`, `-`, `*`, and `/`. Calling any
of these functions with more than two arguments is equivalent to
calling the same function on the first two arguments and then calling
it again on the resulting value and the rest of the arguments. For
example, `(+ 1 2 3)` is equivalent to `(+ (+ 1 2) 3)`. With only one
argument, `+` and `*` return the value; `-` returns its negation and `/` its
reciprocal.

基本的算术操作即加法、减法、乘法和除法，通过函数 `+`、`-`、`*`、`/`
支持所有不同类型的 Lisp
数字。使用超过两个参数来调用这其中任何一个函数等价于在前两个参数上调用相同的函数而后再在所得结果和其余参数上再次调用。例如，`(+ 1 2 3)`
等价于 `(+ (+ 1 2) 3)`。 当只有一个参数时，`+` 和 `*` 直接返回其值，
`-` 返回其相反值，而 `/` 返回其倒数。

```lisp
(+ 1 2)              ==> 3
(+ 1 2 3)            ==> 6
(+ 10.0 3.0)         ==> 13.0
(+ #c(1 2) #c(3 4))  ==> #c(4 6)
(- 5 4)              ==> 1
(- 2)                ==> -2
(- 10 3 5)           ==> 2
(* 2 3)              ==> 6
(* 2 3 4)            ==> 24
(/ 10 5)             ==> 2
(/ 10 5 2)           ==> 1
(/ 2 3)              ==> 2/3
(/ 4)                ==> 1/4
```

If all the arguments are the same type of number (rational, floating
point, or complex), the result will be the same type except in the
case where the result of an operation on complex numbers with rational
components yields a number with a zero imaginary part, in which case
the result will be a rational. However, floating-point and complex
numbers are contagious--if all the arguments are reals but one or more
are floating-point numbers, the other arguments are converted to the
nearest floating-point value in a "largest" floating-point
representation of the actual floating-point arguments. Floating-point
numbers in a "smaller" representation are also converted to the larger
representation. Similarly, if any of the arguments are complex, any
real arguments are converted to the complex equivalents.

如果所有实参都是相同类型的数（有理数、浮点数或复数），则结果也将是同类型的，除非带有有理部分的复数操作的结果产生了一个零虚部的数，此时结果将是一个有理数。尽管如此，浮点数和复数是有传播性的。如果所有实参都是实数但其中有一个或更多是浮点数，那么其他实参将被转化成以实际浮点参数的
“最大” 浮点表示而成的最接近浮点值。那些“较小”表示的浮点数也将被转化成更大的表示。同样，如果实参中的任何一个是复数，则任何实参数会被转化成等价的复数。

```lisp
(+ 1 2.0)             ==> 3.0
(/ 2 3.0)             ==> 0.6666667
(+ #c(1 2) 3)         ==> #c(4 2)
(+ #c(1 2) 3/2)       ==> #c(5/2 2)
(+ #c(1 1) #c(2 -1))  ==> 3
```

Because `/` doesn't truncate, Common Lisp provides four flavors of
truncating and rounding for converting a real number (rational or
floating point) to an integer: **FLOOR** truncates toward negative
infinity, returning the largest integer less than or equal to the
argument. **CEILING** truncates toward positive infinity, returning the
smallest integer greater than or equal to the argument. **TRUNCATE**
truncates toward zero, making it equivalent to **FLOOR** for positive
arguments and to **CEILING** for negative arguments. And **ROUND** rounds to
the nearest integer. If the argument is exactly halfway between two
integers, it rounds to the nearest even integer.

因为 `/` 不作截断处理，所以 Common Lisp 提供了 4
种类型的截断和舍入用于将一个实数（有理数或浮点数）转化成整数：**FLOOR**
向负无穷方向截断，返回小于或等于实参的最大整数；**CEILING**
向正无穷方向截断，返回大于或等于参数的最小整数；**TRUNCATE**
向零截断，对于正实参而言，它等价于 **FLOOR**，而对于负实参则等价于
**CEILING**；而 **ROUND**
舍入到最接近的整数上，如果参数刚好位于两个整数之间，它舍入到最接近的偶数上。

Two related functions are **MOD** and **REM**, which return the modulus and
remainder of a truncating division on real numbers. These two
functions are related to the **FLOOR** and **TRUNCATE** functions as follows:

两个相关的函数是 **MOD** 和
**REM**，它返回两个实数截断相除得到的模和余数。这两个函数与 **FLOOR**
和 **TRUNCATE** 函数之间的关系如下所示：

```lisp
(+ (* (floor    (/ x y)) y) (mod x y)) === x
(+ (* (truncate (/ x y)) y) (rem x y)) === x
```

Thus, for positive quotients they're equivalent, but for negative
quotients they produce different results.

因此，对于正的商它们是等价的，而对于负的商它们产生不同的结果。

The functions `1+` and `1-` provide a shorthand way to express adding and
subtracting one from a number. Note that these are different from the
macros **INCF** and **DECF**. `1+` and `1-` are just functions that return a new
value, but **INCF** and **DECF** modify a place. The following equivalences
show the relation between **INCF**/**DECF**, `1+`/`1-`, and `+`/`-`:

函数 `1+` 和 `1-`
提供了表示从一个数字增加或减少一个的简化方式。注意它们与宏
**INCF** 和 **DECF** 有所不同。`1+` 和 `1-`
只是返回一个新值的函数，而 **INCF** 和 **DECF**
会修改一个位置。下面的恒等式显示了 **INCF**/**DECF**、`1+`/`1-`，和
`+`/`-` 之间的关系：

```lisp
(incf x)    === (setf x (1+ x)) === (setf x (+ x 1))
(decf x)    === (setf x (1- x)) === (setf x (- x 1))
(incf x 10) === (setf x (+ x 10))
(decf x 10) === (setf x (- x 10))
```
