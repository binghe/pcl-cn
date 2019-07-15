# Wrapping Up（总结）

You could keep going, adding more features to this test framework. But
as a framework for writing tests with a minimum of busywork and easily
running them from the REPL, this is a reasonable start. Here's the
complete code, all 26 lines of it:

你可以继续为这个测试框架添加更多特性。但作为一个以最小成本编写测试并可以在
REPL 轻松运行的框架来说，这已经是一个很好的开始了。这里给出完整的代码，全部只有 26 行：

```lisp
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
```

It's worth reviewing how you got here because it's illustrative of how
programming in Lisp often goes.

值得回顾的是，你能走到这一步是因为它显示了 Lisp 编程的一般方式。

You started by defining a simple version of your problem--how to
evaluate a bunch of boolean expressions and find out if they all
returned true. Just **AND**ing them together worked and was syntactically
clean but revealed the need for better result reporting. So you wrote
some really simpleminded code, chock-full of duplication and
error-prone idioms that reported the results the way you wanted.

你从定义一个解决问题的简单版本开始——怎样求值一些布尔表达式并找出它们是否全部返回真。将它们用
**AND** 连在一起可以工作并且在句法上很简洁，但却无法满足以更好的结果输出的需要。因此你写了一些真正简单的代码，其中充满了代码重复以及在用你想要的方式报告结果时容易出错的用法。

The next step was to see if you could refactor the second version into
something as clean as the former. You started with a standard
refactoring technique of extracting some code into a function,
`report-result`. Unfortunately, you could see that using `report-result`
was going to be tedious and error-prone since you had to pass the test
expression twice, once for the value and once as quoted data. So you
wrote the `check` macro to automate the details of calling `report-result`
correctly.

下一步是查看你是否可以将第二个版本重构得跟前面版本一样简洁。你从一个标准的重构技术开始，将某些代码放进函数
`report-result` 中。不幸的是，你发现使用 `report-result`
会产生冗长和易错的代码，由于你不得不将测试表达式传递两次，一次作为值而另一次作为引用的数据。因此，你写了
`check` 宏来自动地正确调用 `report-result` 的细节。

While writing `check`, you realized as long as you were generating code,
you could make a single call to check to generate multiple calls to
report-result, getting you back to a version of test-+ about as
concise as the original AND version.

在编写 `check` 的时候，你认识到在生成代码的同时，也可以让对
`check` 的单一调用生成对 `report-result`
的多个调用，从而得到了一个和最初 **AND** 版本一样简洁的 `test-+` 函数。

At that point you had the `check` API nailed down, which allowed you to
start mucking with how it worked on the inside. The next task was to
fix check so the code it generated would return a boolean indicating
whether all the test cases had passed. Rather than immediately hacking
away at check, you paused to indulge in a little language design by
fantasy. What if--you fantasized--there was already a
non-short-circuiting **AND** construct. Then fixing check would be
trivial. Returning from fantasyland you realized there was no such
construct but that you could write one in a few lines. After writing
combine-results, the fix to check was indeed trivial.

从那里开始你调整了 `check` 的 API，从而你可以开始看到 `check`
内部的工作方式。下一个任务是修正
`check`，使其生成的代码可以返回一个用来指示所有测试用例是否均已通过的布尔值。接下来你没有立即继续玩弄
`check`，而是停下来沉迷于一个设计巧妙的微型语言。你梦想假如有一个非短路的
**AND** 构造就好了，这样修复 `check`
就会非常简单了。回到现实以后，你认识到不存在这样构造，但你可以用几行程序写一个出来。在写出了
`combine-results` 以后，对 `check` 的修复确实简单多了。

At that point all that was left was to make a few more improvements to
the way you reported test results. Once you started making changes to
the test functions, you realized those functions represented a special
category of function that deserved its own abstraction. So you wrote
`deftest` to abstract the pattern of code that turns a regular function
into a test function.

在那一点上唯一剩下的就是对你报告测试结果的方式做一些进一步的改进。一旦你开始修改测试函数，你就会认识到这些函数代表了特殊的一类值得有其自己的抽象方式的函数。因此你写出了
`deftest` 来抽象代码模式，使一个正常函数变成了一个测试函数。

With `deftest` providing an abstraction barrier between the test
definitions and the underlying machinery, you were able to enhance the
result reporting without touching the test functions.

借助 `deftest`
所提供的在测试定义和底层机制之间的抽象障碍，你可以无需修改测试函数而改进汇报结果的方式。

Now, with the basics of functions, variables, and macros mastered, and
a little practical experience using them, you're ready to start
exploring Common Lisp's rich standard library of functions and data
types.

至此，你已经掌握了函数、变量和宏的基础知识，以及一点儿使用它们的实践经验，现在你可以开始探索由函数和数据类型组成的
Common Lisp 丰富的标准库了。
