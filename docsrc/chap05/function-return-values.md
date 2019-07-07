# Function Return Values（函数返回值）

All the functions you've written so far have used the default behavior
of returning the value of the last expression evaluated as their own
return value. This is the most common way to return a value from a
function.

目前写出的所有函数都使用了默认的返回值行为，即最后一个表达式的值被作为整个函数的返回值。这是从函数中返回值的最常见方式。

However, sometimes it's convenient to be able to return from the
middle of a function such as when you want to break out of nested
control constructs. In such cases you can use the **RETURN-FROM** special
operator to immediately return any value from the function.

但某些时候，尤其是想要从嵌套的控制结构中脱身时，如果有办法从函数中间返回。那将是非常便利的。在这种情况下，你可以使用
**RETURN-FROM** 特别操作符，它能够立即以任何值从函数中间返回。

You'll see in Chapter 20 that **RETURN-FROM** is actually not tied to
functions at all; it's used to return from a block of code defined
with the **BLOCK** special operator. However, **DEFUN** automatically wraps
the whole function body in a block with the same name as the
function. So, evaluating a **RETURN-FROM** with the name of the function
and the value you want to return will cause the function to
immediately exit with that value. **RETURN-FROM** is a special operator
whose first "argument" is the name of the block from which to
return. This name isn't evaluated and thus isn't quoted.

在第 20 章将会看到，**RETURN-FROM**
事实上不只用于函数，它还可以被用来从一个由 **BLOCK**
特别操作符所定义的代码块中返回。不过 **DEFUN**
会自动将其整个函数体包装在一个与其函数同名的代码块中。因此，对一个带有当前函数名和想要返回的值的
**RETURN-FROM** 进行求值将导致函数立即以该值退出。**RETURN-FROM**
是一个特殊操作符，其第一个 “参数” 是它想要返回的代码块名。该名字不被求值，因此无需引用。

The following function uses nested loops to find the first pair of
numbers, each less than 10, whose product is greater than the
argument, and it uses **RETURN-FROM** to return the pair as soon as it
finds it:

下面这个函数使用了嵌套循环来发现第一个数对——每个都小于
10，并且其乘积大于函数的参数，它使用 **RETURN-FROM** 在发现之后立即返回该数对：

```lisp
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
```

Admittedly, having to specify the name of the function you're
returning from is a bit of a pain--for one thing, if you change the
function's name, you'll need to change the name used in the
**RETURN-FROM** as well. But it's also the case that explicit **RETURN-FROM**s
are used much less frequently in Lisp than return statements in
C-derived languages, because all Lisp expressions, including control
constructs such as loops and conditionals, evaluate to a value. So
it's not much of a problem in practice.

必须承认的是，不得不指定正在返回的函数名多少会有些不便——比如改变了函数的名字，就需要同时改变
**RETURN-FROM** 中所使用的名字。 但在事实上，显式的
**RETURN-FROM** 调用在 Lisp 中出现的频率远小于 `return`
语句在源自 C 的语言里所出现的频率，因为所有的 Lisp
表达式，包括诸如循环和条件语句这样的控制结构，都会在求值后得到一个返回值。因此在实践中这不是什么问题。
