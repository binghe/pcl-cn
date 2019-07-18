# Sequence Iterating Functions（序列迭代函数）

While in theory all operations on sequences boil down to some
combination of **LENGTH**, **ELT**, and **SETF** of **ELT** operations, Common Lisp
provides a large library of sequence functions.

尽管理论上所有的序列操作都可归结为 **LENGTH**、**ELT**
和 **ELT** 的 **SETF** 操作的某种组合，但 Common Lisp
还是提供了一个庞大的序列函数库。

One group of sequence functions allows you to express certain
operations on sequences such as finding or filtering specific elements
without writing explicit loops. Table 11-1 summarizes them.

有一组序列函数允许你无需编写显式循环就可以表达一些特定序列操作，比如说查找或过滤指定元素等。表
11-1 总结了它们。

> Table 11-1.Basic Sequence Functions

| Name           | Required Arguments | Returns   |
| :------------- | :---------------- | :--------- |
| **COUNT**      | Item and sequence | Number of times item appears in sequence  |
| **FIND**       | Item and sequence | Item or NIL  |
| **POSITION**   | Item and sequence | Index into sequence or NIL  |
| **REMOVE**     | Item and sequence | Sequence with instances of item removed  |
| **SUBSTITUTE** | New item, item, and sequence | Sequence with instances of item replaced with new item |

Here are some simple examples of how to use these functions:

下面是一些关于如何使用这些函数的简单例子：

```lisp
(count 1 #(1 2 1 2 3 1 2 3 4))         ==> 3
(remove 1 #(1 2 1 2 3 1 2 3 4))        ==> #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4))        ==> (2 2 3 2 3 4)
(remove #\a "foobarbaz")               ==> "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) ==> #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) ==> (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz")       ==> "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4))          ==> 1
(find 10 #(1 2 1 2 3 1 2 3 4))         ==> NIL
(position 1 #(1 2 1 2 3 1 2 3 4))      ==> 0
```

Note how **REMOVE** and **SUBSTITUTE** always return a sequence of the same
type as their sequence argument.

注意，**REMOVE** 和 **SUBSTITUTE** 总是返回与其序列实参相同类型的序列。

You can modify the behavior of these five functions in a variety of
ways using keyword arguments. For instance, these functions, by
default, look for elements in the sequence that are the same object as
the item argument. You can change this in two ways: First, you can use
the `:test` keyword to pass a function that accepts two arguments and
returns a boolean. If provided, it will be used to compare item to
each element instead of the default object equality test, **EQL**.
Second, with the `:key` keyword you can pass a one-argument function to
be called on each element of the sequence to extract a key value,
which will then be compared to the item in the place of the element
itself. Note, however, that functions such as **FIND** that return
elements of the sequence continue to return the actual element, not
just the extracted key.

可以使用关键字参数以多种方式修改这五个函数的行为。例如，在默认情况下，这些函数会查看序列中与其项参数相同的对象。你可以用两种方式改变这一行为：首先，你可以使用
`:test`
关键字来传递一个接受两个参数并返回一个布尔值的函数，如果有了这一函数，它将使用该函数代替默认的对象等价性测试
**EQL** 来比较序列中的每个元素； 其次，使用 `:key`
关键字可以传递单参数函数，其被调用在序列的每个元素上以抽取出一个关键值，该值随后会和元素自身的项进行比对。但请注意，诸如
**FIND** 这类返回序列元素的函数将仍然返回实际的元素而不只是被抽取出的关键值。

```lisp
(count "foo" #("foo" "bar" "baz") :test #'string=)    ==> 1
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) ==> (C 30)
```

To limit the effects of these functions to a particular subsequence of
the sequence argument, you can provide bounding indices with `:start`
and `:end` arguments. Passing **NIL** for `:end` or omitting it is the same as
specifying the length of the sequence.

为了将这些函数的效果限制在序列实参的特定子序列上，你可以用 `:start`
和 `:end` 参数提供边界指示。为 `:end` 传递 **NIL**
或是省略它与指定该序列的长度具有相同的效果。

If a non-**NIL** `:from-end` argument is provided, then the elements
of the sequence will be examined in reverse order. By itself `:from-end`
can affect the results of only **FIND** and **POSITION**. For instance:

如果你使用非 **NIL** 的 `:from-end`
参数，那些序列的元素将以相反的顺序被检查。`:from-end`
单独使用只能影响 **FIND** 和 **POSITION** 的结果。例如：

```lisp
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)             ==> (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ==> (A 30)
```

However, the `:from-end` argument can affect **REMOVE** and **SUBSTITUTE** in
conjunction with another keyword parameter, `:count`, that's used to
specify how many elements to remove or substitute. If you specify a
:count lower than the number of matching elements, then it obviously
matters which end you start from:

但 `:from-end` 参数和另一个关键字参数 `:count`
用于指定有多少个元素被移除或替换，这两个参数一起使用时可能影响
**REMOVE** 和 **SUBSTITUTE**
的行为。如果指定了一个低于匹配元素数量的
`:count`，那么从哪一端开始显然至关重要：

```lisp
(remove #\a "foobarbaz" :count 1)             ==> "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t) ==> "foobarbz"
```

And while `:from-end` can't change the results of the **COUNT** function, it
does affect the order the elements are passed to any `:test` and `:key`
functions, which could possibly have side effects. For example:

尽管 `:from-end` 无法改变 **COUNT**
函数的结果，但它确实可以影响传递给任何 `:test` 和 `:key`
函数的元素的顺序，这些函数可能带有副作用。例如：

```lisp
CL-USER> (defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
*V*
CL-USER> (defun verbose-first (x) (format t "Looking at ~s~%" x) (first x))
VERBOSE-FIRST
CL-USER> (count 'a *v* :key #'verbose-first)
Looking at (A 10)
Looking at (B 20)
Looking at (A 30)
Looking at (B 40)
2
CL-USER> (count 'a *v* :key #'verbose-first :from-end t)
Looking at (B 40)
Looking at (A 30)
Looking at (B 20)
Looking at (A 10)
2
```

Table 11-2 summarizes these arguments.

表 11-2 总结了这些参数。

> Table 11-2. Standard Sequence Function Keyword Arguments（标准序列函数关键字参数）

| Argument | Meaning | Default |
| :------- | :------ | :------ |
| `:test` | Two-argument function used to compare item (or value extracted by `:key` function) to element. | **EQL** |
| `:key`| One-argument function to extract key value from actual sequence element. **NIL** means use element as is. | **NIL** |
| `:start` | Starting index (inclusive) of subsequence.	| 0 |
| `:end` | Ending index (exclusive) of subsequence. **NIL** indicates end of sequence. | **NIL** |
| `:from-end` | If true, the sequence will be traversed in reverse order, from end to start. | **NIL** |
| `:count` | Number indicating the number of elements to remove or substitute or **NIL** to indicate all (**REMOVE** and **SUBSTITUTE** only). | **NIL** |


