# Hash Tables（哈希表）

The other general-purpose collection provided by Common Lisp is the
hash table. Where vectors provide an integer-indexed data structure,
hash tables allow you to use arbitrary objects as the indexes, or
keys. When you add a value to a hash table, you store it under a
particular key. Later you can use the same key to retrieve the
value. Or you can associate a new value with the same key--each key
maps to a single value.

Common Lisp
提供的另一个通用集合类型是哈希表。与提供整数索引的数据结构的向量有所不同的是，哈希表允许你使用任意对象作为索引或是键（key）。当你向哈希表添加值时，你把它保存在一个特定的键下保存它。以后就可以使用相同的键来获取该值，或者可以将同一个键关联到一个新值上——每个键映射到单一值上。

With no arguments **MAKE-HASH-TABLE** makes a hash table that considers
two keys equivalent if they're the same object according to **EQL**. This
is a good default unless you want to use strings as keys, since two
strings with the same contents aren't necessarily **EQL**. In that case
you'll want a so-called **EQUAL** hash table, which you can get by passing
the symbol **EQUAL** as the `:test` keyword argument to **MAKE-HASH-TABLE**. Two
other possible values for the `:test` argument are the symbols **EQ** and
EQUALP. These are, of course, the names of the standard object
comparison functions, which I discussed in Chapter 4. However, unlike
the `:test` argument passed to sequence functions, **MAKE-HASH-TABLE**'s
`:test` can't be used to specify an arbitrary function--only the values
**EQ**, **EQL**, **EQUAL**, and **EQUALP**. This is because hash tables actually need
two functions, an equivalence function and a hash function that
computes a numerical hash code from the key in a way compatible with
how the equivalence function will ultimately compare two
keys. However, although the language standard provides only for hash
tables that use the standard equivalence functions, most
implementations provide some mechanism for defining custom hash
tables.

不带参数的 **MAKE-HASH-TABLE**
将创建一个哈希表，其认定两个键等价，当且仅当它们在 **EQL**
的意义上是相同的对象。这是一个好的默认值，除非你想要使用字符串作为健，因为两个带有相同内容的字符串不一定是
**EQL** 等价的。在这种情况下，你需要一个所谓的 **EQUAL**
哈希表，它可以通过将符号 **EQUAL** 作为 `:test`
关键字参数传递给 **MAKE-HASH-TABLE** 来获得。`:test`
参数的另外两个可能的值是符号 **EQ** 和
**EQUALP**。这些都是第 4
章里讨论过的标准对象比较函数的名字。不过，和传递给序列函数的
`:test` 参数不同的是，**MAKE-HASH-TABLE** 的 `:test`
不能被用来指定一个任意函数——只能是值 **EQ**、**EQL**、**EQUAL** 和
**EQUALP**。这是因为哈希表实际上需要两个函数，一个等价性函数以及一个用来从键中以一种和等价函数最终比较两个键时相兼容的方式计算出一个数值的哈希码的哈希函数。不过，尽管语言标准仅提供了使用标准等价函数的哈希表，多数实现都提供了一些自定义哈希表的方法。

The **GETHASH** function provides access to the elements of a hash
table. It takes two arguments--a key and the hash table--and returns
the value, if any, stored in the hash table under that key or **NIL**.
For example:

函数 **GETHASH**
提供了对哈希表元素的访问。它接受两个参数，即键和哈希表，并返回保存在哈希表中相应键下的值（如果有的话），或是
**NIL**。例如：

```lisp
(defparameter *h* (make-hash-table))

(gethash 'foo *h*) ==> NIL

(setf (gethash 'foo *h*) 'quux)

(gethash 'foo *h*) ==> QUUX
```

Since **GETHASH** returns **NIL** if the key isn't present in the table,
there's no way to tell from the return value the difference between a
key not being in a hash table at all and being in the table with the
value **NIL**. **GETHASH** solves this problem with a feature I haven't
discussed yet--multiple return values. **GETHASH** actually returns two
values; the primary value is the value stored under the given key or
**NIL**. The secondary value is a boolean indicating whether the key is
present in the hash table. Because of the way multiple values work,
the extra return value is silently discarded unless the caller
explicitly handles it with a form that can "see" multiple values.

由于当键在表中不存在时 **GETHASH** 返回
**NIL**，所以无法从返回值中看出究竟是键在哈希表中不存在还是键在表中存在却带有值
**NIL**。**GETHASH**
用一个我尚未讨论到的特性解决了这一问题，即通过多重返回值。**GETHASH**
实际上返回两个值：主值是保存在给定键下的值或
**NIL**；从值是一个布尔值，用来指示该键在哈希表中是否存在。由于多重返回值的工作方式，除非调用者用一个可以看见多值的形式显式地处理它，否则额外的返回值将被偷偷地丢掉。

I'll discuss multiple return values in greater detail in Chapter 20,
but for now I'll give you a sneak preview of how to use the
**MULTIPLE-VALUE-BIND** macro to take advantage of **GETHASH**'s extra return
value. **MULTIPLE-VALUE-BIND** creates variable bindings like **LET** does,
filling them with the multiple values returned by a form.

我将在第 20
章里讨论更多关于多重返回值的细节，但目前我将概要地介绍一下如何使用
**MULTIPLE-VALUE-BIND** 宏来利用GETHASH额外返回值。
**MULTIPLE-VALUE-BIND**
创建类似于LET所做的变量绑定，并用一个形式返回的多个值来填充它们。

The following function shows how you might use **MULTIPLE-VALUE-BIND**;
the variables it binds are `value` and `present`:

下面的函数显示了你可以怎样使用
**MULTIPLE-VALUE-BIND**，它绑定的变量是 `value`
和 `present`：

```lisp
(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
      (format nil "Value ~a actually present." value)
      (format nil "Value ~a because key not found." value))))

(setf (gethash 'bar *h*) nil) ; provide an explicit value of NIL

(show-value 'foo *h*) ==> "Value QUUX actually present."
(show-value 'bar *h*) ==> "Value NIL actually present."
(show-value 'baz *h*) ==> "Value NIL because key not found."
```

Since setting the value under a key to **NIL** leaves the key in the
table, you'll need another function to completely remove a key/value
pair. **REMHASH** takes the same arguments as **GETHASH** and removes the
specified entry. You can also completely clear a hash table of all its
key/value pairs with **CLRHASH**.

由于将一个键下面的值设置成 **NIL**
会造成把键留在表中，你需要另一个函数来完全移除一个键值对。**REMHASH**
接受和 **GETHASH**
相同的参数并移除指定的项。你也可以使用 **CLRHASH**
来完全清除哈希表中的所有键值对。

