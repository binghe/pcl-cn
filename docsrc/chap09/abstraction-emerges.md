# An Abstraction Emerges（抽象诞生）

In fixing the test functions, you've introduced several new bits of
duplication. Not only does each function have to include the name of
the function twice--once as the name in the **DEFUN** and once in the
binding of `*test-name*`--but the same three-line code pattern is
duplicated between the two functions. You could remove the duplication
simply on the grounds that duplication is bad. But if you look more
closely at the root cause of the duplication, you can learn an
important lesson about how to use macros.

在修复测试函数的过程中，你又引入了一点儿新的重复。不但每个函数都需要包含其函数名两次——一次作为
**DEFUN** 中的名字，另一次是在 `*test-name*`
绑定里，而且同样的三行代码模式被重复使用在两个函数中。你可以在认定所有的重复都有害这一思路的指导下继续消除这些重复。但如果更进一步地调查一下导致代码重复的根本原因，你就可以学到关于如何使用宏的重要一课。

The reason both these functions start the same way is because they're
both test functions. The duplication arises because, at the moment,
test function is only half an abstraction. The abstraction exists in
your mind, but in the code there's no way to express "this is a test
function" other than to write code that follows a particular pattern.

这两个函数的定义都以相同的方式开始，原因在于它们都是测试函数。导致重复是因为此时测试函数只做了一半抽象。这种抽象存在于你的头脑中，但在代码里没有办法表达
“这是一个测试函数”，除非按照特定的模式来写代码。

Unfortunately, partial abstractions are a crummy tool for building
software. Because a half abstraction is expressed in code by a
manifestation of the pattern, you're guaranteed to have massive code
duplication with all the normal bad consequences that implies for
maintainability. More subtly, because the abstraction exists only in
the minds of programmers, there's no mechanism to make sure different
programmers (or even the same programmer working at different times)
actually understand the abstraction the same way. To make a complete
abstraction, you need a way to express "this is a test function" and
have all the code required by the pattern be generated for you. In
other words, you need a macro.

不幸的是，部分抽象对于构建软件来说是很糟糕的，因为一个半成品的抽象在代码中就是通过模式来表现的，因此必然会得到大量的重复代码，它们将带有一切影响程序可维护性的不良后果。更糟糕的是，因为这种抽象仅存在于程序员的思路之中，所以实际上无法保证不同的程序员（或者甚至是工作在不同时期的同一个程序员）会以同样的方式来理解这种抽象。为了得到一个完整的抽象，你需要用一种方法来表达
“这是一个测试函数”，并且这种方法要能将模式所需的全部代码都生成出来。换句话说，你需要一个宏。

Because the pattern you're trying to capture is a **DEFUN** plus some
boilerplate code, you need to write a macro that will expand into a
**DEFUN**. You'll then use this macro, instead of a plain **DEFUN** to define
test functions, so it makes sense to call it deftest.

由于试图捕捉的模式是一个 **DEFUN**
加上一些样板代码，所以需要写一个宏使其展开成
**DEFUN**。然后使用该宏（而不是用一个简单的
**DEFUN**）去定义测试函数，因此可以将其称为 `deftest`。

```lisp
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
    (let ((*test-name* ',name))
      ,@body)))
```

With this macro you can rewrite `test-+` as follows:

使用该宏你可以像下面这样重写 `test-+`：

```lisp
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```
    
