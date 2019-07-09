# Generalized Assignment（广义赋值）

Variable bindings, of course, aren't the only places that can hold
values. Common Lisp supports composite data structures such as arrays,
hash tables, and lists, as well as user-defined data structures, all
of which consist of multiple places that can each hold a value.

当然，变量绑定并不是唯一可以保留值的位置，Common
Lisp 还支持复合数据结构，包括数组、哈希表、列表以及由用户定义的数据结构，所有这些都含有多个可用来保存值的位置。

I'll cover those data structures in future chapters, but while we're
on the topic of assignment, you should note that **SETF** can assign any
place a value. As I cover the different composite data structures,
I'll point out which functions can serve as "**SETF**able places." The
short version, however, is if you need to assign a value to a place,
**SETF** is almost certainly the tool to use. It's even possible to extend
**SETF** to allow it to assign to user-defined places though I won't cover
that.

后续的章节里将讨论那些数据结构，但就目前所讨论的赋值主题而言，你应该知道
**SETF** 可以为任何位置赋值。当描述不同的复合数据结构时，我将指出哪些函数可以作为
**SETF** 的 “位置” 来使用。总之，如果需要对位置赋值，那么几乎肯定用到
**SETF**。虽然在此不予介绍，但 **SETF** 经拓展后甚至可为由用户定义的位置赋值。

In this regard **SETF** is no different from the `=` assignment operator in
most C-derived languages. In those languages, the `=` operator assigns
new values to variables, array elements, and fields of classes. In
languages such as Perl and Python that support hash tables as a
built-in data type, `=` can also set the values of individual hash table
entries. Table 6-1 summarizes the various ways `=` is used in those
languages.

从这个角度来说，**SETF**和多数源自 C 的语言中的 `=` 赋值操作符没有区别。在那些语言里，`=`
操作符可以将新值赋给变量、数组元素和类的字段。在诸如 Perl
和 Python 这类支持哈希表作为内置数据类型的语言里，`=`
也可以设置哈希表项的值。表 6-1 总结了 `=` 在这些语言里的不同用法。

> *Table 6-1*. Assignment with `=` in Other Languages（表 6-1：其他语
> 言中的 `=` 赋值）

| Assigning to ...       | Java, C, C++    | Perl                  | Python |
| :--------------------- | :-------------- | :-------------------- | :----- |
| ... *variable*         | `x = 10;`       | `$x = 10;`            | `x = 10` |
| ... *array element*    | `a[0] = 10;`    | `$a[0] = 10;`         | `a[0] ` | 
| ... *hash table entry* | `--`            | `$hash{'key'} = 10;`  | `hash['key'] = 10` |
| ... *field in object*  | `o.field = 10;` | `$o->{'field'} = 10;` | `o.field = 10` |

**SETF** works the same way--the first "argument" to **SETF** is a place to
store the value, and the second argument provides the value. As with
the = operator in these languages, you use the same form to express
the place as you'd normally use to fetch the value.17 Thus, the Lisp
equivalents of the assignments in Table 6-1--given that AREF is the
array access function, GETHASH does a hash table lookup, and field
might be a function that accesses a slot named field of a user-defined
object--are as follows:

**SETF** 以同样的方式工作——**SETF**
的第一个参数用来保存值的位置，而第二个参数提供了值。和这些语言中的 `=`
操作符一样，你可以使用和正常获取其值相同的形式来表达位置。  因此，表 6-1
中赋值语句的 Lisp 等价形式分别为：AREF 是数组访问函数，GETHASH
做哈希表查找，而 `field` 可能是一个访问某用户定义对象看名为
`field` 的成员的函数。如下所示：

```
Simple variable:    (setf x 10) 
Array:              (setf (aref a 0) 10)
Hash table:         (setf (gethash 'key hash) 10)
Slot named 'field': (setf (field o) 10)
```

Note that **SETF**ing a place that's part of a larger object has the same
semantics as **SETF**ing a variable: the place is modified without any
effect on the object that was previously stored in the place. Again,
this is similar to how `=` behaves in Java, Perl, and Python.

注意，当到用 **SETF**
赋值对一个作为更大对象一部分的位置进行赋值时，与赋值一个变量具有相同的语义：被修改的位置对之前保存在该位置上的对象没有任何影
响。再一次，这跟 `=` 在 Java、Perl 和 Python中的行为非常相似。
