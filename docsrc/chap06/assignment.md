# Assignment（赋值）

Once you've created a binding, you can do two things with it: get the
current value and set it to a new value. As you saw in Chapter 4, a
symbol evaluates to the value of the variable it names, so you can get
the current value simply by referring to the variable. To assign a new
value to a binding, you use the **SETF** macro, Common Lisp's
general-purpose assignment operator. The basic form of **SETF** is as
follows:

一旦创建了绑定，就可以对它做两件事：获取当前值以及为它设置新值。正如在第 4
章里所看到的，一个符号求值到它所命名的变量的值，因此，可以简单地通过引用这个变量来得到它的当前值。而为绑定赋予新值则要使用
**SETF** 宏——Common
Lisp 的通用赋值操作符。下面是 **SETF** 的基本形式：

```lisp
(setf place value)
```

Because **SETF** is a macro, it can examine the form of the place it's
assigning to and expand into appropriate lower-level operations to
manipulate that `place`. When the place is a variable, it expands into a
call to the special operator SETQ, which, as a special operator, has
access to both lexical and dynamic bindings.15 For instance, to assign
the value 10 to the variable x, you can write this:

因为 **SETF** 是宏，所以它可以检查它所赋值的 `place`
上的形式，并展开成适当的底层操作来修改那个位置，当该位置是变量时，它展开成一个对特殊操作符
**SETQ** 的调用，后者可以访问到词法和动态绑定。 例如，为了将值 10
赋给变量 `x`，可以写成这样：

```lisp
(setf x 10)
```

As I discussed earlier, assigning a new value to a binding has no
effect on any other bindings of that variable. And it doesn't have any
effect on the value that was stored in the binding prior to the
assignment. Thus, the **SETF** in this function:

正如早先所讨论的，为一个绑定赋予新值对该变量的任何其他绑定没有效果。并且它对赋值之前绑定上所保存的值也没有任何效果。因此，函数

```lisp
(defun foo (x) (setf x 10))
```

will have no effect on any value outside of `foo`. The binding that was
created when `foo` was called is set to 10, immediately replacing
whatever value was passed as an argument. In particular, a form such
as the following:

中的 **SETF** 对于 `foo`
之外的任何值都没有效果，这个当 `foo` 被调用时所创建的绑定被设置到
10，立即替换了作为参数传递的任何值。特别是在如下形式中。

```lisp
(let ((y 20))
  (foo y)
  (print y))
```

will print 20, not 10, as it's the value of y that's passed to foo
where it's briefly the value of the variable x before the **SETF** gives x
a new value.

将打印出 20 而不是 10，因为传递给 `foo` 的 `y`
的值在该函数中变成了 `x` 的值，随后又被 **SETF** 设置成新值。

**SETF** can also assign to multiple places in sequence. For instance,
instead of the following:

**SETF** 也可用于依次对多个位置赋值。例如，与其像下面这样：

```lisp
(setf x 1)
(setf y 2)
```

you can write this:

也可以写成这样：

```lisp
(setf x 1 y 2)
```

**SETF** returns the newly assigned value, so you can also nest calls to
**SETF** as in the following expression, which assigns both x and y the
same random value:

**SETF** 返回最近被赋予的值，因此也可以像下面的表达式那样嵌套调用
**SETF**，将 `x` 和 `y` 赋予同一个随机值：

```lisp
(setf x (setf y (random 10)))
```
