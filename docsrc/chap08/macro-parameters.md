# Macro Parameters（宏的形参）

Since the arguments passed to a macro are Lisp objects representing
the source code of the macro call, the first step in any macro is to
extract whatever parts of those objects are needed to compute the
expansion. For macros that simply interpolate their arguments directly
into a template, this step is trivial: simply defining the right
parameters to hold the different arguments is sufficient.

由于传递给宏的实参是代表宏调用源代码的 Lisp
对象，因此任何宏的第一步工作都是提取出那些对象中用于计算展开式的部分。对于那些简单地将其实参直接插入到模板中的宏而言，这一步骤相当简单：只需定义正确的形参来保存不同的实参就可以了。

But this approach, it seems, will not suffice for `do-primes`. The first
argument to the `do-primes` call is a list containing the name of the
loop variable, `p`; the lower bound, 0; and the upper bound, 19. But if
you look at the expansion, the list as a whole doesn't appear in the
expansion; the three element are split up and put in different places.

但是这一方法似乎并不适用于 `do-primes`。`do-primes`
调用的第一个参数是一个列表，其含有循环变量名 `p` 及其下界 0
和上界 19。但如果查看展开式就会发现，该列表作为整体并没有出现在展开式中，三个元素被拆分开并分别放在不同的位置上。

You could define `do-primes` with two parameters, one to hold the list
and a `&rest` parameter to hold the body forms, and then take apart the
list by hand, something like this:

你可以定义带有两个形参的宏 `do-primes`：一个用来保存该列表，另一个
`&rest` 形参保存整个形式的主体，然后手工分拆该列表，类似下面这样：

```lisp
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))
```

In a moment I'll explain how the body generates the correct expansion;
for now you can just note that the variables `var`, `start`, and `end` each
hold a value, extracted from `var-and-range`, that's then interpolated
into the backquote expression that generates `do-primes`'s expansion.

很快我们将解释上述宏形式体是怎样生成正确的展开式的。目前只需注意到变量
`var`、`start` 和 `end` 都持有一个从 `var-and-range`
中提取出的值，它们随后被插入到反引用表达式中以生成 `do-primes` 的展开式。

However, you don't need to take apart `var-and-range` "by hand" because
macro parameter lists are what are called destructuring parameter
lists. Destructuring, as the name suggests, involves taking apart a
structure--in this case the list structure of the forms passed to a
macro.

尽管如此，你并不需要 “手工” 分拆
`var-and-range`，因为宏形参列表是所谓的解构（destructuring）形参列表。顾名思义，“解构”
的意思就是分解一个结构体，在本例中是传递给一个宏的列表结构。

Within a destructuring parameter list, a simple parameter name can be
replaced with a nested parameter list. The parameters in the nested
parameter list will take their values from the elements of the
expression that would have been bound to the parameter the list
replaced. For instance, you can replace `var-and-range` with a list
`(var start end)`, and the three elements of the list will automatically be
destructured into those three parameters.

在解构形参列表中，简单的形参名将被替换成嵌套的形参列表。嵌套形参列表中的形参将从绑定到该形参列表的表达式的元素中获得其值。例如可以将
`var-and-range` 替换成一个列表 `(var start end)`，然后这个列表的三个元素将被自动解构到三个形参上。

Another special feature of macro parameter lists is that you can use
`&body` as a synonym for `&rest`. Semantically `&body` and `&rest` are
equivalent, but many development environments will use the presence of
a `&body` parameter to modify how they indent uses of the
macro--typically `&body` parameters are used to hold a list of forms
that make up the body of the macro.

宏形参列表的另一个特性是可以使用 `&body` 作为 `&rest`
的同义词。`&body` 和 `&rest`
在语义上是等价的，但许多开发环境根据一个 `&body`
形参的存在来修改它们缩进那些使用该宏的代码的方式。通常 `&body`
被用来保存一个构成该宏主体的形式的列表。

So you can streamline the definition of `do-primes` and give a hint to
both human readers and your development tools about its intended use
by defining it like this:

因此你可以通过将 `do-primes` 定义成下面这样来完成其定义，并同时向读者和你的开发工具说明它的用途：

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
```

In addition to being more concise, destructuring parameter lists also
give you automatic error checking--with `do-primes` defined this way,
Lisp will be able to detect a call whose first argument isn't a
three-element list and will give you a meaningful error message just
as if you had called a function with too few or too many
arguments. Also, in development environments such as SLIME that
indicate what arguments are expected as soon as you type the name of a
function or macro, if you use a destructuring parameter list, the
environment will be able to tell you more specifically the syntax of
the macro call. With the original definition, SLIME would tell you
`do-primes` is called like this:

除了更加简洁以外，解构形参列表还可以给你自动检查错误。通过以这种方式定义
`do-primes`，Lisp
可以检测到那些起始参数不是三元素列表的调用并给你一个有意义的错误信息，就好像你用太多或太少的参数调用了一个函数那样。同样，在诸如
SLIME 这样的开发环境中，只要输入一个函数或宏的名字就可以指示它所期待的参数。如果你使用了一个解构形参列表，那么环境将可以更明确地告诉你宏调用的语法。使用最初的定义，SLIME
将告诉你 `do-primes` 可以像这样来调用：

```lisp
(do-primes var-and-range &rest body)
```

But with the new definition, it can tell you that a call should look like this:

但在新定义下，它可以告诉你一个调用应当看起来像这样：

```lisp
(do-primes (var start end) &body body)
```

Destructuring parameter lists can contain `&optional`, `&key`, and `&rest`
parameters and can contain nested destructuring lists. However, you
don't need any of those options to write `do-primes`.

解构形参列表可以含有 `&optional`、`&key` 和 `&rest`
形参，并且可以含有嵌套的解构列表。不过你在编写 `do-primes`
的过程中不需要任何这些选项。
