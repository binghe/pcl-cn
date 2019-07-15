# A Hierarchy of Tests（测试的层次体系）

Now that you've established test functions as first-class citizens,
the question might arise, should test-arithmetic be a test function?
As things stand, it doesn't really matter--if you did define it with
deftest, its binding of `*test-name*` would be shadowed by the bindings
in `test-+` and `test-*` before any results are reported.

由于所建立的测试函数，所以可能会产生一些问题，`test-arithmetic`
应该是一个测试函数吗？事实证明，这件事无关紧要——如果确实用 `deftest`
来定义它，那么它对 `*test-name*`
的绑定将在任何结果被汇报之前被 `test-+` 和 `test-*`
中的绑定所覆盖。

But now imagine you've got thousands of test cases to organize. The
first level of organization is provided by test functions such as
`test-+` and `test-*` that directly call `check`. But with thousands of test
cases, you'll likely need other levels of organization. Functions such
as `test-arithmetic` can group related test functions into test
suites. Now suppose some low-level test functions are called from
multiple test suites. It's not unheard of for a test case to pass in
one context but fail in another. If that happens, you'll probably want
to know more than just what low-level test function contains the test
case.

但是现在，想象你有上千个测试用例需要组织在一起。组织的第一层是由诸如 `test-+`
和 `test-*` 这些能够直接调用 `check`
的测试函数所建立起来的，但在有数千个测试用例的情况下，你将需要其他层面的组织方式。诸如
`test-arithmetic`
这样的函数可以将相关的测试函数组成测试套件。现在假设某些底层测试函数会被多个测试套件所调用。测试用例很有可能在一个上下文中可以通过而在另一个中失败。如果发生了这种事，你的想知道很可能就不仅仅是哪一个底层测试函数含有这个测试用例那么简单了。

If you define the test suite functions such as `test-arithmetic` with
`deftest` and make a small change to the `*test-name*` bookkeeping, you
can have results reported with a "fully qualified" path to the test
case, something like this:

如果用 `deftest` 来定义诸如 `test-arithmetic`
这样的测试套件函数那么简单了，并且对其中的 `*test-name*`
作一个小改变，就可以用测试用例的 “全称”
路径来报告结果，就像下面这样：

```lisp
pass ... (TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
```

Because you've already abstracted the process of defining a test
function, you can change the bookkeeping details without modifying the
code of the test functions. To make `*test-name*` hold a list of test
function names instead of just the name of the most recently entered
test function, you just need to change this binding form:

因为已经抽象了定义测试函数的过程，所以就可以无需修改测试函数的代码从而改变相关的细节。为了使
`*test-name*` 保存一个测试函数名的列表而不只是最近进入的测试函数的名字，你需要将绑定形式

```lisp
(let ((*test-name* ',name))
```

to the following:

变成：

```lisp
(let ((*test-name* (append *test-name* (list ',name))))
```

Since **APPEND** returns a new list made up of the elements of its
arguments, this version will bind `*test-name*` to a list containing the
old contents of `*test-name*` with the new name tacked onto the end.
When each test function returns, the old value of `*test-name*` will be
restored.

由于 **APPEND** 返回一个由其实参元素所构成的新列表，这个版本将把 `*test-name*`
绑定到一个含有追加其新的名字到结尾处的 `*test-name*`
的旧内容的列表。 当每一个测试函数返回时，`*test-name*` 原有的值将被恢复。

Now you can redefine `test-arithmetic` with deftest instead of **DEFUN**.

现在你可以用 `deftest` 代替 **DEFUN** 来重新定义 `test-arithmetic`。

```lisp
(deftest test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))
```

The results now show exactly how you got to each test expression.

现在的结果明确地显示了你是怎样到达每一个测试表达式的。

```lisp
CL-USER> (test-arithmetic)
pass ... (TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
pass ... (TEST-ARITHMETIC TEST-+): (= (+ 1 2 3) 6)
pass ... (TEST-ARITHMETIC TEST-+): (= (+ -1 -3) -4)
pass ... (TEST-ARITHMETIC TEST-*): (= (* 2 2) 4)
pass ... (TEST-ARITHMETIC TEST-*): (= (* 3 5) 15)
T
```

As your test suite grows, you can add new layers of test functions; as
long as they're defined with deftest, the results will be reported
correctly. For instance, the following:

随着你测试套件的增长，你可以添加新的测试函数层次。只要它们用 `deftest`
来定义，结果就会正确地输出。例如下面的定义：

```lisp
(deftest test-math ()
  (test-arithmetic))
```

would generate these results:

将生成这样的结果：

```lisp
CL-USER> (test-math)
pass ... (TEST-MATH TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
pass ... (TEST-MATH TEST-ARITHMETIC TEST-+): (= (+ 1 2 3) 6)
pass ... (TEST-MATH TEST-ARITHMETIC TEST-+): (= (+ -1 -3) -4)
pass ... (TEST-MATH TEST-ARITHMETIC TEST-*): (= (* 2 2) 4)
pass ... (TEST-MATH TEST-ARITHMETIC TEST-*): (= (* 3 5) 15)
T
```
