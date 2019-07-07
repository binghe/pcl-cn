# Mixing Different Parameter Types（混合不同的形参类型）

It's possible, but rare, to use all four flavors of parameters in a
single function. Whenever more than one flavor of parameter is used,
they must be declared in the order I've discussed them: first the
names of the required parameters, then the optional parameters, then
the rest parameter, and finally the keyword parameters. Typically,
however, in functions that use multiple flavors of parameters, you'll
combine required parameters with one other flavor or possibly combine
`&optional` and `&rest` parameters. The other two combinations, either
`&optional` or `&rest` parameters combined with `&key` parameters, can lead
to somewhat surprising behavior.

在单一函数里使用所有四种类型形参的情况虽然罕见，但也是可能的。无论何时，当用到多种类型的形参时，它们必须以这样的顺序声明：首先是必要形参，其次是可选形参，再次是剩余形参，最后才是关键字形参。但在使用多种类型形参的函数中，一般情况是将必要形参和另外一种类型的形参组合使用，或者可能是组合
`&optional` 形参和 `&rest` 形参。其他两种组合方式，无论是 `&optional`
形参还是 `&rest` 形参，当与 `&key` 形参组合使用时，都可能导致某种奇怪的行为。

Combining `&optional` and `&key` parameters yields surprising enough
results that you should probably avoid it altogether. The problem is
that if a caller doesn't supply values for all the optional
parameters, then those parameters will eat up the keywords and values
intended for the keyword parameters. For instance, this function
unwisely mixes `&optional` and `&key` parameters:

将 `&optional` 形参和 `&key`
形参组合使用时会产生令人惊奇的结果，因此也许应该避免将它们一起使用。问题出在如果调用者没有为所有可选形参提供值时，那么没有得到值的可选形参将吃掉原本用于关键字形参的关键字和值。例如，下面这个函数很不明智地混合了
`&optional` 和 `&key` 形参：

```lisp
(defun foo (x &optional y &key z) (list x y z))
```

If called like this, it works fine:

如果像这样调用的话，就没问题：

```lisp
(foo 1 2 :z 3) ==> (1 2 3)
```

And this is also fine:

这样也可以：

```lisp
(foo 1)  ==> (1 nil nil)
```

But this will signal an error:

但是这样的话将会报错：

```lisp
(foo 1 :z 3) ==> ERROR
```

This is because the keyword `:z` is taken as a value to fill the
optional `y` parameter, leaving only the argument 3 to be processed. At
that point, Lisp will be expecting either a keyword/value pair or
nothing and will complain. Perhaps even worse, if the function had had
two `&optional` parameters, this last call would have resulted in the
values `:z` and 3 being bound to the two `&optional` parameters and the
`&key` parameter `z` getting the default value **NIL** with no indication that
anything was amiss.

这是因为关键字 `:z` 被作为一个值填入到可选的 `y`
形参中了，只留下了参数 3 被处理。在这里，Lisp
期待一个成对的键/值，或者什么也没有，否则就会报错。也许更坏的是，如果该函数带有两个
`&optional` 形参，上面最后一个调用将导致值 `:z` 和 3
分别被绑定到两个 `&optional` 形参上，而 `&key` 形参 `z`
将得到默认值 **NIL**，而不声明缺失了东西。

In general, if you find yourself writing a function that uses both
`&optional` and `&key` parameters, you should probably just change it to
use all `&key` parameters--they're more flexible, and you can always add
new keyword parameters without disturbing existing callers of the
function. You can also remove keyword parameters, as long as no one is
using them. In general, using keyword parameters helps make code much
easier to maintain and evolve--if you need to add some new behavior to
a function that requires new parameters, you can add keyword
parameters without having to touch, or even recompile, any existing
code that calls the function.

一般而言，如果正在编写一个同时使用 `&optional` 形参和
`&key` 形参的函数，可能就应该将它变成全部使用 `&key`
形参的形式——它们更灵活，并且总会可以在不破坏该函数的已有调用的情况下添加新的关键字形参。也可以移除关键字形参，只要没人在使用它们。 一般而言，使用关键字形参将会使代码相对易于维护和拓展——如果需要为函数添加一些需要用到新参数的新行为，就可以直接添加关键字形参，而无需修改甚至重新编译任何调用该函数的已有代码。

You can safely combine `&rest` and `&key` parameters, but the behavior may
be a bit surprising initially. Normally the presence of either `&rest`
or `&key` in a parameter list causes all the values remaining after the
required and `&optional` parameters have been filled in to be processed
in a particular way--either gathered into a list for a `&rest` parameter
or assigned to the appropriate `&key` parameters based on the
keywords. If both `&rest` and `&key` appear in a parameter list, then both
things happen--all the remaining values, which include the keywords
themselves, are gathered into a list that's bound to the `&rest`
parameter, and the appropriate values are also bound to the `&key`
parameters. So, given this function:

虽然可以安全地组合使用 `&rest` 形参和 `&key`
形参，但其行为初看起来可能会有一点奇怪。正常地来讲，无论是 `&rest` 还是
`&key` 出现在形参列表中，都将导致所有出现在必要形参和 `&optional`
形参之后的那些值被特别处理——要么作为 `&rest`
形参被收集到一个形参列表中，要么基于关键字被分配到适当的 `&key`
形参中。如果 `&rest` 和 `&key`
同时出现在形参列表中，那么两件事都会发生——所有剩余的值，包括关键字本身，都将被收集到一个列表里，然后被绑定到
`&rest` 形参上；而适当的值，也会同时被绑定到 `&key` 形参上。因此，给定下列函数：

```lisp
(defun foo (&rest rest &key a b c) (list rest a b c))
```

you get this result:

你将得到如下结果：

```lisp
(foo :a 1 :b 2 :c 3)  ==> ((:A 1 :B 2 :C 3) 1 2 3)
```
