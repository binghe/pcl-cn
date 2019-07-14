# Practical: Building a Unit Test Framework（实践：构建一个单元测试框架）

In this chapter you'll return to cutting code and develop a simple
unit testing framework for Lisp. This will give you a chance to use
some of the features you've learned about since Chapter 3, including
macros and dynamic variables, in real code.

在本章里，你将编写代码为 Lisp
开发一个简单的单元测试框架。这将使你有机会在真实代码中使用从第 3
章起已学到的某些语言特性，包括宏和动态变量。

The main design goal of the test framework will be to make it as easy
as possible to add new tests, to run various suites of tests, and to
track down test failures. For now you'll focus on designing a
framework you can use during interactive development.

该测试框架的主要设计目标是使其可以尽可能简单地增加新测试，运行多个测试套件，以及跟踪测试的失败。目前，你将集中于设计一个可以在交互开发期间使用的框架。

The key feature of an automated testing framework is that the
framework is responsible for telling you whether all the tests
passed. You don't want to spend your time slogging through test output
checking answers when the computer can do it much more quickly and
accurately. Consequently, each test case must be an expression that
yields a boolean value--true or false, pass or fail. For instance, if
you were writing tests for the built-in `+` function, these might be
reasonable test cases:

一个自动测试框架的关键特性在于该框架应该能够告诉你是否所有的测试都通过了。当计算机可以处理得更快更精确时，你就不应该将时间花在埋头检查测试所输出的答案上。因此，每个测试用例必须是一个能产生布尔值的表达式——真或假，通过或失败。举个例子，如果正在为内置的 `+`
函数编写测试，那么下面这些可能是合理的测试用例：

```lisp
(= (+ 1 2) 3)
(= (+ 1 2 3) 6)
(= (+ -1 -3) -4)
```

Functions that have side effects will be tested slightly
differently--you'll have to call the function and then check for
evidence of the expected side effects. But in the end, every test case
has to boil down to a boolean expression, thumbs up or thumbs down.

带有副作用的函数会以稍微不同的方式进行测试。你必须调用该函数，然后查找是否有证据表明存在着预期的副作用。 但最终，每个测试用例都将归结为一个布尔表达式，要么真要么假。
