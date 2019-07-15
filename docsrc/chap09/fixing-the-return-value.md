# Fixing the Return Value（修复返回值）

You can start with fixing `test-+` so its return value indicates whether
all the test cases passed. Since check is responsible for generating
the code that ultimately runs the test cases, you just need to change
it to generate code that also keeps track of the results.

接下来可以修复 `test-+`
以使其返回值可以指示所有测试用例是否都通过了。由于
`check` 负责生成最终用来运行测试用例的代码，所以只需改变它来生成可以同时跟踪结果的代码就可以了。

As a first step, you can make a small change to `report-result` so it
returns the result of the test case it's reporting.

首先可以对 `report-result`
做一个小改变，以使其在报告时顺便返回测试用例结果。

```lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
```

Now that `report-result` returns the result of its test case, it might
seem you could just change the **PROGN** to an **AND** to combine the
results. Unfortunately, **AND** doesn't do quite what you want in this
case because of its short-circuiting behavior: as soon as one test
case fails, **AND** will skip the rest. On the other hand, if you had a
construct that worked like **AND** without the short-circuiting, you could
use it in the place of **PROGN**, and you'd be done. Common Lisp doesn't
provide such a construct, but that's no reason you can't use it: it's
a trivial matter to write a macro to provide it yourself.

现在 `report-result` 返回了它的测试用例结果，故而看起来只需将 **PROGN**
变成 **AND** 就可以组合结果了。不幸的是，由于其短路行为的存在，**AND**
在本例中并不能完成你想要的事：一旦某个测试用例失败了，**AND**
将跳过其余的测试。另一方面，如果有一个像
**AND** 那样运作的操作符，同时又没有短路行为，那么就可以用它来代替
**PROGN**，从而事情也就完成了。虽然 Common Lisp
并不提供这样一种构造，但你没有理由不能使用它：自己编写提供这一功能的宏是极其简单的。

Leaving test cases aside for a moment, what you want is a macro--let's
call it `combine-results`--that will let you say this:

暂时把测试用例放在一边，所需要的宏应如下所示，我们称其为
`combine-results`：

```lisp
(combine-results
  (foo)
  (bar)
  (baz))
```

and have it mean something like this:

它应该与下列代码等价：

```lisp
(let ((result t))
  (unless (foo) (setf result nil))
  (unless (bar) (setf result nil))
  (unless (baz) (setf result nil))
  result)
```

The only tricky bit to writing this macro is that you need to
introduce a variable--`result` in the previous code--in the
expansion. As you saw in the previous chapter, using a literal name
for variables in macro expansions can introduce a leak in your macro
abstraction, so you'll need to create a unique name. This is a job for
`with-gensyms`. You can define `combine-results` like this:

编写这个宏唯一麻烦之处在于，需要在展开式中引入一个变量，即前面代码中的
`result`。但正如你从前面章节所看到的那样，在宏展开式中使用一个变量的字面名称会导致宏抽象出现漏洞，因此需要创建一个唯一的名字，这就需要用到
`with-gensyms` 了。可以像下面这样来定义 `combine-results`：

```lisp
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))
```

Now you can fix `check` by simply changing the expansion to use
`combine-results` instead of **PROGN**.

现在可以通过简单地改变展开式用 `combine-results` 代替
**PROGN** 来修复 `check`。

```lisp
(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))
```

With that version of check, `test-+`
should emit the results of its three test expressions and then return
**T** to indicate that everything passed.

使用这个版本的 `check`，`test-+` 就可以输出它的三个测试表达式结果，并返回
**T** 以表明每一个测试都通过了。

```lisp
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
T
```

And if you change one of the test cases so it fails,
the final return value changes to **NIL**.

并且如果你改变里一个测试用例使其失败 ，最终的返回值也将变成 **NIL**。

```lisp
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
FAIL ... (= (+ -1 -3) -5)
NIL
```
