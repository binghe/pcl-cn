# Plugging the Leaks（堵住漏洞）

In his essay "The Law of Leaky Abstractions," Joel Spolsky coined the
term leaky abstraction to describe an abstraction that "leaks" details
it's supposed to be abstracting away. Since writing a macro is a way
of creating an abstraction, you need to make sure your macros don't
leak needlessly.

Jeol Spolsky 在他的随笔 “The Law of Leaky Abstractions”
里创造了术语 “有漏洞的抽象”（leaky abstraction），以此来描述一种抽象，其
“泄露” 了本该抽象掉的细节。由于编写宏是一种创造抽象的方式，故此你需要确保宏不产生不必要的泄露。

As it turns out, a macro can leak details of its inner workings in
three ways. Luckily, it's pretty easy to tell whether a given macro
suffers from any of those leaks and to fix them.

如同即将看到的，宏可能以三种方式泄露其内部工作细节。幸运的是，可以相当容易地看出一个给定的宏是否存在着任何一种泄露方式并修复它。

The current definition suffers from one of the three possible macro
leaks: namely, it evaluates the `end` subform too many times. Suppose
you were to call `do-primes` with, instead of a literal number such as
19, an expression such as `(random 100)` in the `end` position.

当前的宏定义存在三种可能的宏泄露中的一种：确切地说，它会过多地对
`end` 子形式求值。假设没有使用诸如 19 这样的字面数字，而是用像
`(random 100)` 这样的表达式在 `end` 的位置上来调用 `do-primes`：

```lisp
(do-primes (p 0 (random 100))
  (format t "~d " p))
```

Presumably the intent here is to loop over the primes from zero to
whatever random number is returned by `(random 100)`. However, this
isn't what the current implementation does, as **MACROEXPAND-1** shows.

假设这里的意图是要在从 0 到 `(random 100)`
所返回的任意随机数字的范围内循环查找素数。但 **MACROEXPAND-1**
的结果显示这并不是当前实现所做的事。

```lisp
CL-USER> (macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
    ((> P (RANDOM 100)))
  (FORMAT T "~d " P))
T
```

When this expansion code is run, **RANDOM** will be called each time the
end test for the loop is evaluated. Thus, instead of looping until p
is greater than an initially chosen random number, this loop will
iterate until it happens to draw a random number less than or equal to
the current value of `p`. While the total number of iterations will
still be random, it will be drawn from a much different distribution
than the uniform distribution **RANDOM** returns.

当我们运行展开式代码时，**RANDOM**
将在每次进行循环的终止测试时被求值一次。这样，循环将不会在p大于一个初始给定的随机数时终止，而是在循环刚好生成一个小于或等于当前 `p`
值的随机数时，循环才会终止。由于循环的整体次数仍然是随机的，因此它将产生一个与
**RANDOM** 所返回的统一分布相当不同的分布形式。

This is a leak in the abstraction because, to use the macro correctly,
the caller needs to be aware that the end form is going to be
evaluated more than once. One way to plug this leak would be to simply
define this as the behavior of do-primes. But that's not very
satisfactory--you should try to observe the Principle of Least
Astonishment when implementing macros. And programmers will typically
expect the forms they pass to macros to be evaluated no more times
than absolutely necessary. Furthermore, since `do-primes` is built on
the model of the standard macros, **DOTIMES** and **DOLIST**, neither of which
causes any of the forms except those in the body to be evaluated more
than once, most programmers will expect do-primes to behave similarly.

这就是一种抽象中的漏洞，因为为了正确使用该宏，调用者必须注意 `end`
形式被求值超过一次的情况。一种堵住漏洞的方式是简单地将其定义成 `do-primes`
的行为。但这并不是非常令人满意的，你在实现宏时应当试图遵守最少惊动原则（Principle
of Least Astonishment）。而且通常情况下，程序员们希望它们传递给宏的形式除非必要将不会被多次求值。 更进一步，由于
`do-primes` 是构建在标准宏 **DOTIMES** 和 **DOLIST**
之上的，而这两个宏都不会导致其循环体之外的形式被多次求值，所以多数程序员将期待
`do-primes` 具有相似的行为。

You can fix the multiple evaluation easily enough; you just need to
generate code that evaluates end once and saves the value in a
variable to be used later. Recall that in a **DO** loop, variables defined
with an initialization form and no step form don't change from
iteration to iteration. So you can fix the multiple evaluation problem
with this definition:

修复多重求值问题是相当容易的：只需生成代码来对
`end` 只求值一次，并将其值保存在一个稍后将会用到的变量里。回想在
**DO**
循环中，用一个初始形式但没有步长形式来定义的变量并不会在迭代过程中改变其值。因此可以用下列定义来修复多重求值问题：

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))
```

Unfortunately, this fix introduces two new leaks to the macro
abstraction.

然而不幸的是，这一修复却又给宏抽象引入了两个新漏洞。

One new leak is similar to the multiple-evaluation leak you just
fixed. Because the initialization forms for variables in a **DO** loop are
evaluated in the order the variables are defined, when the macro
expansion is evaluated, the expression passed as `end` will be evaluated
before the expression passed as `start`, opposite to the order they
appear in the macro call. This leak doesn't cause any problems when
`start` and `end` are literal values like 0 and 19. But when they're forms
that can have side effects, evaluating them out of order can once
again run afoul of the Principle of Least Astonishment.

其中一个新漏洞类似于刚修复的多重求值漏洞。因为在 **DO**
循环中，变量的初始形式是以变量被定义的顺序来求值的，当宏展开被求值时，传递给
`end` 的表达式将在传递给 `start`
的表达式之前求值，这与它们出现在宏调用中的顺序相反。并在 `start`
和 `end` 都是像 0 和 19
这样的字面数值时，这一泄露不会带来任何问题。但当它们是可以产生副作用的形式时，不同的求值顺序将使它们再次违反最少惊动原则。

This leak is trivially plugged by swapping the order of the two
variable definitions.

通过交换两个变量的定义顺序就可轻易堵上该漏洞。

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))
```

The last leak you need to plug was created by using the variable name
`ending-value`. The problem is that the name, which ought to be a purely
internal detail of the macro implementation, can end up interacting
with code passed to the macro or in the context where the macro is
called. The following seemingly innocent call to do-primes doesn't
work correctly because of this leak:

最后一个需要堵上的漏洞是由于使用了变量名 `ending-value`
而产生的。问题在于这个名字（其应当完全属于宏实现内部的细节）它可以跟传递给宏的代码或是宏被调用的上下文产生交互。下面这个看似无辜的
`do-primes` 调用会由于这个漏洞而无法正常工作：

```lisp
(do-primes (ending-value 0 10)
  (print ending-value))
```

Neither does this one:

下面这个也不行：

```lisp
(let ((ending-value 0))
  (do-primes (p 0 10)
    (incf ending-value p))
  ending-value)
```

Again, **MACROEXPAND-1** can show you the problem. The first call
expands to this:

再一次，**MACROEXPAND-1** 向你展示了问题所在。第一个调用展开成这样：

```lisp
(do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
     (ending-value 10))
    ((> ending-value ending-value))
  (print ending-value))
```

Some Lisps may reject this code because `ending-value` is used twice as
a variable name in the same **DO** loop. If not rejected outright, the
code will loop forever since `ending-value` will never be greater than
itself.

某些 Lisp 可能因为 `ending-value` 作为变量名在同一个 **DO**
循环中被用了两次而拒绝上面的代码。如果没有被完全拒绝，上述代码也将无限循环下去，由于
`ending-value` 永远不会大于其自身。

The second problem call expands to the following:

第二个问题调用展开成下面的代码：

```lisp
(let ((ending-value 0))
  (do ((p (next-prime 0) (next-prime (1+ p)))
       (ending-value 10))
      ((> p ending-value))
    (incf ending-value p))
  ending-value)
```

In this case the generated code is perfectly legal, but the behavior
isn't at all what you want. Because the binding of `ending-value`
established by the **LET** outside the loop is shadowed by the variable
with the same name inside the **DO**, the form `(incf ending-value p)`
increments the loop variable ending-value instead of the outer
variable with the same name, creating another infinite loop.

在这种情况下生成的代码是完全合法的，但其行为完全不是你想要的那样。由于在循环之外由
**LET** 所建立的 `ending-value` 绑定被 **DO**
内部同名的变量所掩盖，形式 `(incf ending-value p)`
将递增循环变量 `ending-value` 而不是同名的外层变量，因此得到了另一个无限循环。

Clearly, what you need to patch this leak is a symbol that will never
be used outside the code generated by the macro. You could try using a
really unlikely name, but that's no guarantee. You could also protect
yourself to some extent by using packages, as described in
Chapter 21. But there's a better solution.

很明显，为了补上这个漏洞，需要一个永远不会在宏展开代码之外被用到的符号。可以尝试使用一个真正罕用的名字，但即便如此也不可能做到万无一失。也可以使用第
21 章里介绍的包（package），从而在某种意义上起到保护作用。但还有一个更好的解决方案。

The function **GENSYM** returns a unique symbol each time it's
called. This is a symbol that has never been read by the Lisp reader
and never will be because it isn't interned in any package. Thus,
instead of using a literal name like `ending-value`, you can generate a
new symbol each time `do-primes` is expanded.

函数 **GENSYM**
在其每次被调用时返回唯一的符号。这是一个没有被 Lisp
读取器读过的符号并且永远不会被读到，因为它不会进入到任何包里。因而就可以在每次
`do-primes` 被展开时生成一个新的符号以替代像 `ending-value` 这样的字面名称。

```lisp
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
```

Note that the code that calls **GENSYM** isn't part of the expansion; it
runs as part of the macro expander and thus creates a new symbol each
time the macro is expanded. This may seem a bit strange at
first--`ending-value-name` is a variable whose value is the name of
another variable. But really it's no different from the parameter var
whose value is the name of a variable--the difference is the value of
var was created by the reader when the macro form was read, and the
value of ending-value-name is generated programmatically when the
macro code runs.

注意调用 **GENSYM**
的代码并不是展开式的一部分，它作为宏展开器的一部分来运行从而在每次宏被展开时创建一个新符号。这初看起来有一点奇怪——`ending-value-name`
是一个变量，其值是另一个变量名。但其实它和值为一个变量名的形参 `var`
并没有什么区别，区别在于 `var`
的值是由读取器在宏调用在读取时创建的，而 `ending-value-name`
的值则是在宏代码运行时由程序化生成的。

With this definition the two previously problematic forms expand into
code that works the way you want. The first form:

使用这个定义，前面两个有问题的形式现在就可以展开成按预想方式运作的代码了。第一个形式：

```lisp
(do-primes (ending-value 0 10)
  (print ending-value))
```

expands into the following:

展开成下面的代码：

```lisp
(do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
     (#:g2141 10))
    ((> ending-value #:g2141))
  (print ending-value))
```

Now the variable used to hold the ending value is the gensymed symbol,
`#:g2141`. The name of the symbol, `G2141`, was generated by **GENSYM** but
isn't significant; the thing that matters is the object identity of
the symbol. Gensymed symbols are printed in the normal syntax for
uninterned symbols, with a leading `#:`.

现在用来保存循环终值的变量是生成符号，`#:g2141`。该符号的名字 `G2141`
是由 **GENSYM**
所生成的，这并不重要，重要的是这个符号的对象标识。生成符号是以未保留符号通常的语法形式打印出来的，带有前缀 `#:`。

The other previously problematic form:

另一个之前有问题的形式：

```lisp
(let ((ending-value 0))
  (do-primes (p 0 10)
    (incf ending-value p))
  ending-value)
```

looks like this if you replace the `do-primes` form with its expansion:

如果将 `do-primes` 形式替换成其展开式的话，以上形式将会变成这样：

```lisp
(let ((ending-value 0))
  (do ((p (next-prime 0) (next-prime (1+ p)))
       (#:g2140 10))
      ((> p #:g2140))
    (incf ending-value p))
  ending-value)
```

Again, there's no leak since the ending-value variable bound by the
**LET** surrounding the `do-primes` loop is no longer shadowed by any
variables introduced in the expanded code.

再一次，由于 `do-primes` 循环外围的 **LET**
所绑定的变量 `ending-value`
不再被任何由展开代码引入的变量所掩盖，因此再没有漏洞了。

Not all literal names used in a macro expansion will necessarily cause
a problem--as you get more experience with the various binding forms,
you'll be able to determine whether a given name is being used in a
position that could cause a leak in a macro abstraction. But there's
no real downside to using a gensymed name just to be safe.

并非宏展开式中用到的所有字面名称都会导致问题。等你对于多种绑定形式有了更多经验以后，你将可以鉴别一个用在某个位置上的给定名字是否会导致在宏抽象中出现漏洞。但安全起见，使用一个符号生成的名字并没有什么坏处。

With that fix, you've plugged all the leaks in the implementation of
do-primes. Once you've gotten a bit of macro-writing experience under
your belt, you'll learn to write macros with these kinds of leaks
preplugged. It's actually fairly simple if you follow these rules of
thumb:

利用这些修复就可以堵上 `do-primes`
实现中的所有漏洞了。一旦积累了一点宏编写方面的经验以后，你将获得在预先堵上这几类漏洞的情况下编写宏的本领。事实上做到这点很容易，只须遵循下面所概括的这些规则即可。

* Unless there's a particular reason to do otherwise, include any
subforms in the expansion in positions that will be evaluated in the
same order as the subforms appear in the macro call.

* Unless there's a particular reason to do otherwise, make sure
subforms are evaluated only once by creating a variable in the
expansion to hold the value of evaluating the argument form and then
using that variable anywhere else the value is needed in the
expansion.

* Use **GENSYM** at macro expansion time to create variable names used in the expansion.

* 除非有特殊理由，否则需要将展开式中的任何子形式放在一个位置上，使其求值顺序与宏调用的子形式相同。
* 除非有特殊理由，否则需要确保子形式仅被求值一次，方法是在展开式中创建变量来持有求值参数形式所得到的值，然后在展开式中所有需要用到该值的地方使用这个变量。
* 在宏展开期使用 **GENSYM** 来创建展开式中用到的变量名。
