# Hash Table Iteration（哈希表迭代）

Common Lisp provides a couple ways to iterate over the entries in a
hash table. The simplest of these is via the function
**MAPHASH**. Analogous to the **MAP** function, **MAPHASH** takes a two-argument
function and a hash table and invokes the function once for each
key/value pair in the hash table. For instance, to print all the
key/value pairs in a hash table, you could use **MAPHASH** like this:

Common Lisp
提供了几种在哈希表项上迭代的方式。其中最简单的方式是通过函数
**MAPHASH**。和 **MAP** 函数相似，**MAPHASH**
接受一个两参数函数和一个哈希表并在哈希表的每一个键值对上调用一次该函数。例如，为了打印哈希表中所有的键值对，你可以像这样来使用 **MAPHASH**：

```lisp
(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
```

The consequences of adding or removing elements from a hash table
while iterating over it aren't specified (and are likely to be bad)
with two exceptions: you can use **SETF** with **GETHASH** to change the value
of the current entry, and you can use **REMHASH** to remove the current
entry. For instance, to remove all the entries whose value is less
than ten, you could write this:

在迭代一个哈希表的过程中，向其中添加或移除元素的后果没有被指定（并且可能会很坏），但却有两个例外：你可以将
**SETF** 与 **GETHASH**
一起使用来改变当前项的值，并且可以使用 **REMHASH**
来移除当前项。例如，为了移除所有其值小于 10 的项，你可以写成下面这样：

```lisp
(maphash #'(lambda (k v) (when (< v 10) (remhash k *h*))) *h*)
```

The other way to iterate over a hash table is with the extended **LOOP**
macro, which I'll discuss in Chapter 22. The **LOOP** equivalent of the
first **MAPHASH** expression would look like this:

另一种在哈希表上迭代的方式是使用扩展的 **LOOP** 宏，我将在第 22
章里讨论它。第一个 **MAPHASH** 表达式的等价 **LOOP** 形式如下所示：

```lisp
(loop for k being the hash-keys in *h* using (hash-value v)
  do (format t "~a => ~a~%" k v))
```

I could say a lot more about the nonlist collections supported by
Common Lisp. For instance, I haven't discussed multidimensional
arrays at all or the library of functions for manipulating bit
arrays. However, what I've covered in this chapter should suffice
for most of your general-purpose programming needs. Now it's finally
time to look at Lisp's eponymous data structure: lists.

关于 Common Lisp
所支持的非列表集合，我还可以讲更多的内容。例如，多维数组以及处理位数组的函数库。但本章中涉及到的内容将满足多数通用编程场合的需要。现在，终于可以介绍列表这个让
Lisp 因此得名的数据结构了。
