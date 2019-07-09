# DO

While **DOLIST** and **DOTIMES** are convenient and easy to use, they aren't
flexible enough to use for all loops. For instance, what if you want
to step multiple variables in parallel? Or use an arbitrary expression
to test for the end of the loop? If neither **DOLIST** nor **DOTIMES** meet
your needs, you still have access to the more general **DO** loop.

尽管 **DOLIST** 和 **DOTIMES**
方便且易于使用，但却没有灵活到可用于所有循环。例如，如果想要并行循环多个变量该怎样做？或是使用任意表达式来测试循环的末尾呢？如果
**DOLIST** 和 **DOTIMES** 都不能满足需求，那还可以用更通用的 **DO** 循环。

Where **DOLIST** and **DOTIMES** provide only one loop variable, **DO** lets you
bind any number of variables and gives you complete control over how
they change on each step through the loop. You also get to define the
test that determines when to end the loop and can provide a form to
evaluate at the end of the loop to generate a return value for the **DO**
expression as a whole. The basic template looks like this:

与 **DOLIST** 和 **DOTIMES** 只提供一个循环变量有所不同的是，**DO**
允许绑定任意数量的变量，也可以定义测试条件来决定何时终止循环，并可以提供一个形式，在循环结束时进行求值来为
**DO** 表达式整体生成一个返回值。基本模板如下所示：

```lisp
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)
```

Each `variable-definition` introduces a variable that will be in scope
in the body of the loop. The full form of a single variable definition
is a list containing three elements.

每个 `variable-definition`
引入了一个将存在于循环体作用域之内的变量。单个变量定义的完整形式是一个含有三个元素的列表。

```lisp
(var init-form step-form)
```

The `init-form` will be evaluated at the beginning of the loop and the
resulting values bound to the variable `var`. Before each subsequent
iteration of the loop, the `step-form` will be evaluated and the new
value assigned to `var`. The `step-form` is optional; if it's left out,
the variable will keep its value from iteration to iteration unless
you explicitly assign it a new value in the loop body. As with the
variable definitions in a **LET**, if the `init-form` is left out, the
variable is bound to **NIL**. Also as with **LET**, you can use a plain
variable name as shorthand for a list containing just the name.

上述 `init-form` 在循环开始时被求值并将结果值绑定到变量 `var`
上。在循环的每一个后续迭代开始之前，`step-form` 将被求值并把新值分配给
`var`。`step-form` 是可选的，如果它没有给出，那么变量将在迭代过程中保持其值不变，除非在循环体中显式地为其赋予新值。和
**LET** 中的变量定义一样，如果 `init-form` 没有给出，那么变量将被绑定到
**NIL**。另外和 **LET**
的情形一样的是，你可以将一个只含有名字的列表简化成一个简单的变量名来使用。

At the beginning of each iteration, after all the loop variables have
been given their new values, the `end-test-form` is evaluated. As long
as it evaluates to **NIL**, the iteration proceeds, evaluating the
`statement`s in order.

在每次迭代开始时以及所有循环变量都被指定新值后，`end-test-form`
会被求值。只要其值为 **NIL**，迭代过程就会继续，依次求值所有的 `statement`。

When the `end-test-form` evaluates to true, the `result-form`s are
evaluated, and the value of the last result form is returned as the
value of the **DO** expression.

当 `end-test-form` 求值为真时，`result-form`
将被求值，且最后一个结果形式的值将被作为 **DO** 表达式的值返回。

At each step of the iteration the step forms for all the variables are
evaluated before assigning any of the values to the variables. This
means you can refer to any of the other loop variables in the step
forms. That is, in a loop like this:

在迭代的每一步里，所有变量的 `step-form`
将在分配任何值给变量之前被求值。这意味着可以在步长形式里引用其他任何循环变量。 比如在下列循环中：

```lisp
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
```

the step forms `(1+ n)`, `next`, and `(+ cur next)` are all evaluated
using the old values of `n`, `cur`, and `next`. Only after all the step
forms have been evaluated are the variables given their new
values. (Mathematically inclined readers may notice that this is a
particularly efficient way of computing the eleventh Fibonacci
number.)

其步长形式 `(1+ n)`、`next` 和 `(+ cur next)` 均使用 `n`、`cur` 和
`next` 的旧值来求值。只有当所有步长形式都被求值以后，这些变量才被指定其新的值。（有数学天赋的读者可能会注意到，这其实是一种计算第
11 个斐波那契数的特别有效的方式。）

This example also illustrates another characteristic of **DO**--because
you can step multiple variables, you often don't need a body at
all. Other times, you may leave out the result form, particularly if
you're just using the loop as a control construct. This flexibility,
however, is the reason that **DO** expressions can be a bit cryptic. Where
exactly do all the parentheses go? The best way to understand a **DO**
expression is to keep in mind the basic template.

这个例子还阐述了 **DO**
的另一种特征——由于可以同时推进多个变量所以往往根本不需要一个循环体。其他时候，尤其在只是把循环用作控制构造时，则可能会省略结果形式。尽管如此，这种灵活性正是
**DO** 表达式有点儿晦涩难懂的原因。所有这些括号都该放在哪里？理解一个
**DO** 表达式的最佳方式是记住其基本模板：

```lisp
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)
```

The six parentheses in that template are the only ones required by the
**DO** itself. You need one pair to enclose the variable declarations, one
pair to enclose the end test and result forms, and one pair to enclose
the whole expression. Other forms within the **DO** may require their own
parentheses--variable definitions are usually lists, for instance. And
the test form is often a function call. But the skeleton of a **DO** loop
will always be the same. Here are some example **DO** loops with the
skeleton in bold:

该模板中的六个括号是 **DO**
结构本身所必需的。一对括号来围住变量声明，一对用来围住终止测试形式和结果形式，以及一对用来围住整个表达式。**DO**
中的其他形式可能需要它们自己的括号——例如变量定义总是以列表形式存在，而测试形式则通常是一个函数调用。不过
**DO** 循环的框架将总是一致的。下面是一些框架用黑体表示的 **DO** 循环的例子。

```lisp
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))
```

Notice that the result form has been omitted. This is, however, not a
particularly idiomatic use of **DO**, as this loop is much more simply
written using **DOTIMES**.

注意到本例的结果被忽略了。不过这种用法对 **DO**
来说没有特别意义，因为用 **DOTIMES** 来写这个循环会更简单。

```lisp
(dotimes (i 4) (print i))
```

As another example, here's the bodiless Fibonacci-computing loop:

另一个例子是一个没有循环体的斐波那契数计算循环：

```lisp
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
```

Finally, the next loop demonstrates a **DO** loop that binds no
variables. It loops while the current time is less than the value of a
global variable, printing "Waiting" once a minute. Note that even with
no loop variables, you still need the empty variables list.

最后，下面循环演示了一个不绑定变量的 **DO**
循环。在当前时间小于一个全局变量值的时候，它保持循环，每分钟打印一个
“Waiting”。注意，就算没有循环变量，仍然需要有那个空变量列表。

```lisp
(do ()
    ((> (get-universal-time) *some-future-date*))
  (format t "Waiting~%")
  (sleep 60)) 
```
