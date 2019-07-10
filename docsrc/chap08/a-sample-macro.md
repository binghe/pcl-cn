# A Sample Macro: `do-primes`（一个示例宏：`do-primes`）

To see how this three-step process works, you'll write a macro
`do-primes` that provides a looping construct similar to **DOTIMES** and
**DOLIST** except that instead of iterating over integers or elements of a
list, it iterates over successive prime numbers. This isn't meant to
be an example of a particularly useful macro--it's just a vehicle for
demonstrating the process.

为了观察这三步过程是怎样发挥作用的，下面将编写一个叫做 `do-primes`
的宏，它提供了一个类似 DOTIMES 和 DOLIST
的循环结构，只是它并非迭代在整数或者一个列表的元素上，而是迭代在相继的素数上。这并非意味着这是一个特别有用的宏，只是为了演示宏的定义过程罢了。

First, you'll need two utility functions, one to test whether a given
number is prime and another that returns the next prime number greater
or equal to its argument. In both cases you can use a simple, but
inefficient, brute-force approach.

首先你需要两个工具函数：一个用来测试给定的数是否为素数，另一个用来返回大于或等于其实参的下一个素数。这两种情况都可以使用简单而低效的暴力手法来解决。

```lisp
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
```

Now you can write the macro. Following the procedure outlined
previously, you need at least one example of a call to the macro and
the code into which it should expand. Suppose you start with the idea
that you want to be able to write this:

现在就可以写这个宏了。按照前面所概括的过程，至少需要一个宏调用示例以及它应当展开成的代码。假设开始时想通过如下代码：

```lisp
(do-primes (p 0 19)
  (format t "~d " p))
```
  
to express a loop that executes the body once each for each prime
number greater or equal to 0 and less than or equal to 19, with the
variable p holding the prime number. It makes sense to model this
macro on the form of the standard **DOLIST** and **DOTIMES** macros; macros
that follow the pattern of existing macros are easier to understand
and use than macros that introduce gratuitously novel syntax.

来表达一个循环，在每个大于等于 0 并小于等于 19
的素数上分别依次执行循环体，并以变量 `p` 保存当前素数。仿照标准
**DOLIST** 和 **DOTIMES**
宏来定义是合理的。按照已有宏的模式操作的宏比那些引入了无谓的新颖语法的宏更易于理解和使用。

Without the `do-primes` macro, you could write such a loop with **DO** (and
the two utility functions defined previously) like this:

如果没有 `do-primes` 宏，你可以用
**DO**（和前面定义的两个工具函数）来写出下面这个循环：

```lisp
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))
```

Now you're ready to start writing the macro code that will translate
from the former to the latter.

现在就可以开始编写将前者转化成后者的代码了。
