# Two First Tries（两个最初的尝试）

If you were doing ad hoc testing, you could enter these expressions at
the REPL and check that they return **T**. But you want a framework that
makes it easy to organize and run these test cases whenever you
want. If you want to start with the simplest thing that could possibly
work, you can just write a function that evaluates the test cases and
**AND**s the results together.

如果正在做探索测试，那么就可以在REPL中输入这些表达式并检查它们是否返回
**T**。但你可能想要一个框架使其可以在需要时轻松地组织和运行这些测试用例。如果想先处理最简单的可行情况，那就可以只写一个函数，让它对所有的测试用例都予以求值并用
**AND** 将结果连在一起

```lisp
(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```

Whenever you want to run this set of test cases, you can call `test-+`.

无论何时，当想要运行这组测试用例时，都可以调用 `test-+`。

```lisp
CL-USER> (test-+)
T
```

As long as it returns **T**, you know the test cases are passing. This way
of organizing tests is also pleasantly concise--you don't have to
write a bunch of test bookkeeping code. However, as you'll discover
the first time a test case fails, the result reporting leaves
something to be desired. When `test-+` returns **NIL**, you'll know
something failed, but you'll have no idea which test case it was.

一旦它返回
**T**，就可知道测试用例通过了。这种组织测试的方式也很优美简洁——不需要编写大量的重复测试代码。然而一旦发现某个测试用例失败了，同样也会发现它的运行报告会遗漏一些有用的信息。当
`test-+` 返回 **NIL**
时，你会知道某些测试失败了，但却不会知道这究竟是哪一个测试用例。

So let's try another simple--even simpleminded--approach. To find out
what happens to each test case, you could write something like this:

因此，让我们来尝试另一种简单得甚至有些愚蠢的方法。为了找出每个测试用例的运行情况，你可以写成类似下面这样：

```lisp
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

Now each test case will be reported individually. The `~:[FAIL~;pass~]`
part of the **FORMAT** directive causes **FORMAT** to print "FAIL" if the
first format argument is false and "pass" otherwise. Then you label
the result with the test expression itself. Now running `test-+` shows
you exactly what's going on.

现在每个测试用例都将单独报告结果。**FORMAT** 指令中的 `~:[FAIL~;pass~]`
部分将导致FORMAT在其第一个格式实参为假时打印出 “FAIL”，而在其他情况下为
“pass”。 然后会将测试表达式本身标记到结果上。现在运行 `test-+`
就可以明确显示出发生了什么事。

```lisp
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
NIL
```

This time the result reporting is more like what you want, but the
code itself is pretty gross. The repeated calls to **FORMAT** as well as
the tedious duplication of the test expression cry out to be
refactored. The duplication of the test expression is particularly
grating because if you mistype it, the test results will be
mislabeled.

这次的结果报告更像是你想要的，可是代码本身却一团糟。问题出在对 **FORMAT**
的重复调用以及测试表达式乏味的重复，这些急切需要被重构。其中测试表达式的重复尤其讨厌，因为如果发生了错误输入，测试结果就会被错误地标记。

Another problem is that you don't get a single indicator whether all
the test cases passed. It's easy enough, with only three test cases,
to scan the output looking for "FAIL"; however, when you have hundreds
of test cases, it'll be more of a hassle.

另一个问题在于，你无法得到单一的关于所有测试是否都通过的指示。如果只有三个测试用例的话，很容易通过扫描输出并查找
“FAIL” 来看到这点。不过当有几百个测试用例时，这将非常困难。
