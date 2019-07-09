# Other Ways to Modify Places（其他修改位置的方式）

While all assignments can be expressed with **SETF**, certain patterns
involving assigning a new value based on the current value are
sufficiently common to warrant their own operators. For instance,
while you could increment a number with **SETF**, like this:

尽管所有的赋值都可以用 **SETF**
来表达，但有些固定模式（比如例基于当前值来赋予新值）由于经常使用，因此有它们自己的操作符。例如，尽管可以像这样使用
**SETF** 来递增一个数：

```lisp
(setf x (+ x 1))
```

or decrement it with this:

或是像这样来递减它：

```lisp
(setf x (- x 1))
```

it's a bit tedious, compared to the C-style `++x` and `--x`. Instead, you
can use the macros **INCF** and **DECF**, which increment and decrement a
place by a certain amount that defaults to 1.

但这跟 C 风格的 `++x` 和 `--x` 相比就显得很冗长了。相反，可以使用宏 **INCF**
和 **DECF**，它们以默认为 1 的特定数量对一个位置的值进行递增和递减。

```lisp
(incf x)    === (setf x (+ x 1))
(decf x)    === (setf x (- x 1))
(incf x 10) === (setf x (+ x 10))
```

**INCF** and **DECF** are examples of a kind of macro called _modify
macros_. Modify macros are macros built on top of **SETF** that modify
places by assigning a new value based on the current value of the
place. The main benefit of modify macros is that they're more concise
than the same modification written out using **SETF**. Additionally,
modify macros are defined in a way that makes them safe to use with
places where the place expression must be evaluated only once. A silly
example is this expression, which increments the value of an arbitrary
element of an array:

类似 **INCF** 和 **DECF** 这种宏被称为_修改宏_（modify macro），修改宏是建立在
**SETF** 之上的宏，其基于作用位置上的当前值来赋予该位置一个新值，修改宏的
主要好处是，它们比用
**SETF** 写出的同样修改语句更加简洁。另外，修改宏所定义的方式使其可以安全地用于那些表达式必须只被求值一次的位置。一个有趣的例子是下面这个表达式，其中的
**INCF** 会递增一个数组中任意元素的值：

```lisp
(incf (aref *array* (random (length *array*))))
```

A naive translation of that into a **SETF** expression might look like
this:

如果将它就地转换成一个 **SETF** 表达式可能看起来像这样：

```lisp
(setf (aref *array* (random (length *array*)))
      (1+ (aref *array* (random (length *array*)))))
```

However, that doesn't work because the two calls to **RANDOM** won't
necessarily return the same value--this expression will likely
grab the value of one element of the array, increment it, and
then store it back as the new value of a different element. The
**INCF** expression, however, does the right thing because it knows
how to take apart this expression:
      
但这不会正常工作，因为两次对 **RANDOM**
的调用不一定能返回相同的值——该表达式将很可能抓取数组中一个元素的值，将其递增，然后将其作为新值保存到另一个不同的数组元素上。与之相比，上面的
**INCF** 表达式却能产生正确的行为，因为它知道如何处理这个表达式：

```lisp
(aref *array* (random (length *array*)))
```

to pull out the parts that could possibly have side effects to make
sure they're evaluated only once. In this case, it would probably
expand into something more or less equivalent to this:

取出其中可能带有副作用的部分，从而确保它们仅被求值一次。在本例中，经展
开后，它差不多会等价于以下形式。

```lisp
(let ((tmp (random (length *array*))))
  (setf (aref *array* tmp) (1+ (aref *array* tmp))))
```

In general, modify macros are guaranteed to evaluate both their
arguments and the subforms of the place form exactly once each, in
left-to-right order.

一般而言，修改宏可以保证以从左到右的顺序，对它们的参数和位置形式的子形式每个只求值一次。

The macro **PUSH**, which you used in the mini-database to add elements to
the `*db*` variable, is another modify macro. You'll take a closer look
at how it and its counterparts **POP** and **PUSHNEW** work in Chapter 12 when
I talk about how lists are represented in Lisp.

第 3 章里那个微型数据库中曾用来向
`*db*` 变量添加元素的 **PUSH** 宏则是另一个修改宏。第 12
章在讲到如何在 Lisp 中表示列表时会详细地介绍它及其对应的 **POP**
和 **PUSHNEW** 是如何工作的。

Finally, two slightly esoteric but useful modify macros are **ROTATEF**
and **SHIFTF**. **ROTATEF** rotates values between places. For instance, if
you have two variables, a and b, this call:

最后有两个稍微有些难懂但很有用的修改宏，它们是 **ROTATEF** 和 **SHIFTF**。**ROTATEF**
在位置之前旋转它们的值。如果有两个变量 `a` 和 `b`，那么如下调用

```lisp
(rotatef a b)
```

swaps the values of the two variables and returns NIL. Since a and b
are variables and you don't have to worry about side effects, the
previous **ROTATEF** expression is equivalent to this:

将交换两个变量的值并返回NIL。由于 `a` 和 `b`
是变量并且你不需要担心副作用，因此前面的 **ROTATEF** 表达式等价于下面这个：

```lisp
(let ((tmp a)) (setf a b b tmp) nil)
```

With other kinds of places, the equivalent expression using **SETF** would
be quite a bit more complex.

其他类型位置上的 **SETF** 等价表达式可能会更加复杂一些。

**SHIFTF** is similar except instead of rotating values it shifts them to
the left--the last argument provides a value that's moved to the
second-to-last argument while the rest of the values are moved one to
the left. The original value of the first argument is simply
returned. Thus, the following:

**SHIFTF** 与之相似，除了它将值向左侧移动而不是旋转它们——最后一个参数提供了一个值用来移动到最数第二个参数上，而其他的值将向左移动一个，第一个参数
的最初的值将被简单地返回。这样，下面的表达式

```lisp
(shiftf a b 10)
```

is equivalent--again, since you don't have to worry about side effects--to this:

将等价于如下形式。同样，不必担心副作用：

```lisp
(let ((tmp a)) (setf a b b 10) tmp)
```

Both **ROTATEF** and **SHIFTF** can be used with any number of arguments and,
like all modify macros, are guaranteed to evaluate them exactly once,
in left to right order.

**ROTATEF** 和 **SHIFTF**
都可被用于任意多个参数，并且和所有的修改宏一样，它们可以保证以从左到右的顺序对每个参数仅求值一次。

With the basics of Common Lisp's functions and variables under your
belt, now you're ready to move onto the feature that continues to
differentiate Lisp from other languages: macros.

学完了 Common Lisp 函数和变量的基础知识以后，下面将开始介绍一个令
Lisp 始终区别于其他语言的重要特性：宏。
