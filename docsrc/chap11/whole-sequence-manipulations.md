# Whole Sequence Manipulations（针对整个序列的操作）

A handful of functions perform operations on a whole sequence (or
sequences) at a time. These tend to be simpler than the other
functions I've described so far. For instance, **COPY-SEQ** and **REVERSE**
each take a single argument, a sequence, and each returns a new
sequence of the same type. The sequence returned by **COPY-SEQ** contains
the same elements as its argument while the sequence returned by
**REVERSE** contains the same elements but in reverse order. Note that
neither function copies the elements themselves--only the returned
sequence is a new object.

有一些函数可以一次在整个序列（或多个序列）上进行操作。这些函数比目前为止我已描述的其他函数简单一些。例如，**COPY-SEQ**
和 **REVERSE** 都接受单一的序列参数并返回一个相同类型的新序列。**COPY-SEQ**
返回的序列包含与其参数相同的元素，而 **REVERSE**
返回的序列则含有顺序相反的相同元素。注意，这两个函数都不会复制元素本身，只有返回的序列是一个新对象。

The **CONCATENATE** function creates a new sequence containing the
concatenation of any number of sequences. However, unlike **REVERSE** and
**COPY-SEQ**, which simply return a sequence of the same type as their
single argument, **CONCATENATE** must be told explicitly what kind of
sequence to produce in case the arguments are of different types. Its
first argument is a type descriptor, like the `:element-type` argument
to **MAKE-ARRAY**. In this case, the type descriptors you'll most likely
use are the symbols **VECTOR**, **LIST**, or **STRING**. For example:

函数 **CONCATENATE**
创建一个将任意数量序列连接在一起的新序列。不过，跟 **REVERSE** 和 **COPY-SEQ**
简单地返回与其单一参数相同类型序列有所不同的是，**CONCATENATE**
必须被显式指定产生何种类型的序列，因为其参数可能是不同类型的。它的第一个参数是类型描述符，就像是
**MAKE-ARRAY** 的 `:element-type`
参数那样。这里最常用到的类型描述符是符号 **VECTOR**、**LIST**
和 **STRING**。 例如：

```lisp
(concatenate 'vector #(1 2 3) '(4 5 6))    ==> #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6))      ==> (1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) ==> "abcdef"
```
