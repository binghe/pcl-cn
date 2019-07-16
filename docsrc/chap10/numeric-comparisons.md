# Numeric Comparisons（数值比较）

The function `=` is the numeric equality predicate. It compares numbers
by mathematical value, ignoring differences in type. Thus, `=` will
consider mathematically equivalent values of different types
equivalent while the generic equality predicate **EQL** would consider
them inequivalent because of the difference in type. (The generic
equality predicate **EQUALP**, however, uses = to compare numbers.) If
it's called with more than two arguments, it returns true only if they
all have the same value. Thus:

函数 `=`
是数值等价谓词。它用数学意义上的值来比较数字，而忽略类型上的区别。这样，`=`
将把不同类型在数学意义上等价的值视为等价，而通用等价谓词 **EQL**
将由于其类型差异而视其不等价。（但通用等价谓词 **EQUALP**
使用 `=` 来比较数字。）如果它以超过两个参数被调用，它将只有当所有参数具有相同值时才返回真。如下所示：

```lisp
(= 1 1)                        ==> T
(= 10 20/2)                    ==> T
(= 1 1.0 #c(1.0 0.0) #c(1 0))  ==> T
```

The `/=` function, conversely, returns true only if all its arguments
are different values.

相反，函数 `/=` 只有当它的全部参数都是不同值才返回真。

```lisp
(/= 1 1)        ==> NIL
(/= 1 2)        ==> T
(/= 1 2 3)      ==> T
(/= 1 2 3 1)    ==> NIL
(/= 1 2 3 1.0)  ==> NIL
```

The functions `<`, `>`, `<=`, and `>=` order rationals and floating-point
numbers (in other words, the real numbers.) Like `=` and `/=`, these
functions can be called with more than two arguments, in which case
each argument is compared to the argument to its right.

函数 `<`、`>`、`<=` 和 `>=` 检查有理数和浮点数（也就是实数）的次序。跟 `=`
和 `/=` 相似，这些函数也可以用超过两个参数来调用，这时每个参数都跟其右边的那个参数相比较。

```lisp
(< 2 3)       ==> T
(> 2 3)       ==> NIL
(> 3 2)       ==> T
(< 2 3 4)     ==> T
(< 2 3 3)     ==> NIL
(<= 2 3 3)    ==> T
(<= 2 3 3 4)  ==> T
(<= 2 3 4 3)  ==> NIL
```

To pick out the smallest or largest of several numbers, you can use
the function **MIN** or **MAX**, which takes any number of real number
arguments and returns the minimum or maximum value.

要想选出几个数字中最小或最大的那个，你可以使用函数 **MIN** 或
**MAX**，其接受任意数量的实数参数并返回最小或最大值。

```lisp
(max 10 11)    ==> 11
(min -12 -10)  ==> -12
(max -1 2 -3)  ==> 2
```

Some other handy functions are **ZEROP**, **MINUSP**, and **PLUSP**, which test
whether a single real number is equal to, less than, or greater than
zero. Two other predicates, **EVENP** and **ODDP**, test whether a single
integer argument is even or odd. The `P` suffix on the names of these
functions is a standard naming convention for predicate functions,
functions that test some condition and return a boolean.

其他一些常用函数包括 **ZEROP**、**MINUSP** 和
**PLUSP**，用来测试单一实数是否等于、小于或大于零。另外两个谓词
**EVENP**
和 **ODDP**，测试单一整数参数是否是偶数或奇数。这些函数名称中的P后缀是一种谓词函数的标准命名约定，这些函数能够测试某些条件并返回一个布尔值。

