# Refactoring（重构）

What you'd really like is a way to write test functions as streamlined
as the first `test-+` that return a single **T** or **NIL** value but that also
report on the results of individual test cases like the second
version. Since the second version is close to what you want in terms
of functionality, your best bet is to see if you can factor out some
of the annoying duplication.

我们真正所需要的编辑方式应该是可以写出像第一个 `test-+`
那样能够返回单一的 **T** 或 **NIL**
值的高效函数，但同时它还可以像第二个版本那样能够报告单独测试用例的结果。就功能而言，由于第二个版本更接近于预期结果，所以最好看看能否可以将某些烦人的重复消除掉。

The simplest way to get rid of the repeated similar calls to **FORMAT** is
to create a new function.

消除重复的 **FORMAT**
相似调用的最简单方法就是创建一个新函数。

```lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))
```

Now you can write `test-+` with calls to `report-result` instead of
**FORMAT**. It's not a huge improvement, but at least now if you
decide to change the way you report results, there's only one place
you have to change.

现在就可以用 `report-result` 来代替 **FORMAT**
编写 `test-+`
了。这不是一个大的改进，但至少现在如果打算改变报告结果的方式，则只需要修改一处即可。

```lisp
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

Next you need to get rid of the duplication of the test case
expression, with its attendant risk of mislabeling of results. What
you'd really like is to be able to treat the expression as both code
(to get the result) and data (to use as the label). Whenever you want
to treat code as data, that's a sure sign you need a macro. Or, to
look at it another way, what you need is a way to automate writing the
error-prone `report-result` calls. You'd like to be able to say
something like this:

接下来需要摆脱的是测试用例表达式的重复以及由此带来的错误标记结果的风险。真正想要的应该是可以将表达式同时看作代码（为了获得结果）和数据（用来作为标签）。无论何时，若想将代码作为数据来对待，这就意味着肯定需要一个宏。或者从另外一个角度来看，你所需要的方法应该能够自动编写容易出错的
`report-result` 调用。代码可能要写成下面这样：

```lisp
(check (= (+ 1 2) 3))
```

and have it mean the following:

并要让其与下列形式的含义等同：

```lisp
(report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
```

Writing a macro to do this translation is trivial.

很容易就可以写出一个宏来作这种转换。

```lisp
(defmacro check (form)
  `(report-result ,form ',form))
```

Now you can change `test-+` to use `check`.

现在就可以改变 `test-+` 来使用 `check` 了。

```lisp
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
```

Since you're on the hunt for duplication, why not get rid of those
repeated calls to check? You can define check to take an arbitrary
number of forms and wrap them each in a call to `report-result`.

既然不喜欢重复的代码，那为什么不将那些对 `check`
的重复调用也一并消除掉呢？你可以定义 `check`
来接受任意数量的形式并将它们中的每个都封装在一个对 `report-result`
的调用中。

```lisp
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))
```

This definition uses a common macro idiom of wrapping a **PROGN**
around a series of forms in order to turn them into a single
form. Notice also how you can use `,@` to splice in the result of an
expression that returns a list of expressions that are themselves
generated with a backquote template.

这个定义使用了一种常见的宏习惯用法，将一系列打算转化成单一形式的形式分装在一个
**PROGN** 之中。注意是怎样使用 `,@`
将反引用模板所生成的表达式列表嵌入到结果表达式之中的。

With the new version of `check` you can write a new version of `test-+`
like this:

用 `check` 的新版本就可以写出一个像下面这样新版本的 `test-+`：

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

that is equivalent to the following code:

其等价于下面的代码：

```lisp
(defun test-+ ()
  (progn
    (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
    (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4))))
```

Thanks to `check`, this version is as concise as the first version of
`test-+` but expands into code that does the same thing as the second
version. And now any changes you want to make to how `test-+` behaves,
you can make by changing `check`.

多亏有了 `check`，这个版本才和 `test-+`
的第一个版本一样简洁，而其展开代码却与第二个版本有着相同的功能。并且现在若想对
`test-+` 的行为做出任何改变，也都可以通过改变 `check` 来做到。
