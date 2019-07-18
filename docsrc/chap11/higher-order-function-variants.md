# Higher-Order Function Variants（高阶函数变体）

For each of the functions just discussed, Common Lisp provides two
higher-order function variants that, in the place of the item
argument, take a function to be called on each element of the
sequence. One set of variants are named the same as the basic function
with an **-IF** appended. These functions count, find, remove, and
substitute elements of the sequence for which the function argument
returns true. The other set of variants are named with an **-IF-NOT**
suffix and count, find, remove, and substitute elements for which the
function argument does not return true.

对于每个刚刚讨论过的函数，Common Lisp
都提供了两种高阶函数变体，它们接受一个将在每个序列元素上调用的函数，以此来代替项参数。一组变体被命名为与基本函数相同的名字并带有一个追加的
**-IF**。这些函数用于计数、查找、移除以及替换序列中那些函数参数返回真的元素。另一类变体以
**-IF-NOT** 后缀命名并计数、查找、移除以及替换函数参数不返回真的元素。

```lisp
(count-if #'evenp #(1 2 3 4 5))         ==> 2

(count-if-not #'evenp #(1 2 3 4 5))     ==> 3

(position-if #'digit-char-p "abcd0001") ==> 4

(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
  #("foo" "bar" "baz" "foom")) ==> #("foo" "foom")
```

According to the language standard, the **-IF-NOT** variants are
deprecated. However, that deprecation is generally considered to have
itself been ill-advised. If the standard is ever revised, it's more
likely the deprecation will be removed than the **-IF-NOT**
functions. For one thing, the **REMOVE-IF-NOT** variant is probably used
more often than **REMOVE-IF**. Despite its negative-sounding name,
**REMOVE-IF-NOT** is actually the positive variant--it returns the
elements that do satisfy the predicate.

根据语言标准，这些 **-IF-NOT**
变体已经过时了。但这种过时通常被认为是由于标准本身欠考虑。不过，如果标准被再次修订的话，更有可能被去掉的是
**-IF** 而非 **-IF-NOT** 系列。比如说，有个叫 **REMOVE-IF-NOT**
变体就比 **REMOVE-IF** 更经常被使用。尽管它有一个听起来具有否定意义的名字，但
**REMOVE-IF-NOT** 实际上是一个具有肯定意义的变体——它返回满足谓词的那些元素。

The **-IF** and **-IF-NOT** variants accept all the same keyword arguments as
their vanilla counterparts except for `:test`, which isn't needed since
the main argument is already a function. With a `:key` argument, the
value extracted by the `:key` function is passed to the function instead
of the actual element.

除了 `:test`，这些 **-IF** 和 **-IF-NOT**
变体都接受和它们的原始版本相同的关键字参数，`:test`
不再被需要是因为主参数已经是一个函数了。 通过使用 `:key`
参数，由 `:key` 函数所抽取出的值将代替实际元素传递给该函数。

```lisp
(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)     ==> 2

(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) ==> 3

(remove-if-not #'alpha-char-p
  #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0))) ==> #("foo" "bar")
```

The **REMOVE** family of functions also support a fourth variant,
**REMOVE-DUPLICATES**, that has only one required argument, a sequence,
from which it removes all but one instance of each duplicated
element. It takes the same keyword arguments as **REMOVE**, except for
`:count`, since it always removes all duplicates.

**REMOVE** 函数家族还支持第四个变体
**REMOVE-DUPLICATES**，其接受一个序列作为唯一的必要参数，并将其中每个重复的元素移除到只剩下一个实例。它和
**REMOVE** 接受相同的关键字参数，除了
`:count`，因为它总是删除所有的重复。

```lisp
(remove-duplicates #(1 2 1 2 3 1 2 3 4)) ==> #(1 2 3 4)
```
