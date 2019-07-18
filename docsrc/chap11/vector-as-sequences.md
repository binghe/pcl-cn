# Vectors As Sequences（作为序列的向量）

As mentioned earlier, vectors and lists are the two concrete subtypes
of the abstract type sequence. All the functions I'll discuss in the
next few sections are sequence functions; in addition to being
applicable to vectors--both general and specialized--they can also be
used with lists.

正如早先所提到的，向量和列表是抽象类型序列的两种具体子类型。接下来几节里讨论的所有函数都是序列函数：除了可以应用于向量（无论是通用还是特化的）之外，它们还可应用于列表。

The two most basic sequence functions are **LENGTH**, which returns the
length of a sequence, and **ELT**, which allows you to access individual
elements via an integer index. **LENGTH** takes a sequence as its only
argument and returns the number of elements it contains. For vectors
with a fill pointer, this will be the value of the fill pointer. **ELT**,
short for element, takes a sequence and an integer index between zero
(inclusive) and the length of the sequence (exclusive) and returns the
corresponding element. **ELT** will signal an error if the index is out of
bounds. Like **LENGTH**, **ELT** treats a vector with a fill pointer as having
the length specified by the fill pointer.

两个最基本的序列函数是
**LENGTH**，其返回一个序列的长度；**ELT**，其允许通过一个整数索引来访问个别元素。**LENGTH**
接受序列作为其唯一的参数并返回它含有的元素数量。对于带有填充指针的向量，这些是填充指针的值。**ELT**
是元素（element）的简称，它接受序列和从 0
到序列长度（左闭右开区间）的整数索引并返回对应的元素。**ELT**
将在索引超出边界时报错。和 **LENGTH** 一样，**ELT**
也将把一个带有填充指针的向量视为其具有该填充指针所指定的长度。

```lisp
(defparameter *x* (vector 1 2 3))

(length *x*) ==> 3
(elt *x* 0)  ==> 1
(elt *x* 1)  ==> 2
(elt *x* 2)  ==> 3
(elt *x* 3)  ==> error
```

**ELT** is also a **SETF**able place, so you can set the value of a
particular element like this:

**ELT** 也是一个支持 **SETF** 的位置，因此可以像这样来设置一个特定元素的值：

```lisp
(setf (elt *x* 0) 10)

*x* ==> #(10 2 3)
```
