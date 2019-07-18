# Vectors（向量）

Vectors are Common Lisp's basic integer-indexed collection, and they
come in two flavors. Fixed-size vectors are a lot like arrays in a
language such as Java: a thin veneer over a chunk of contiguous memory
that holds the vector's elements. Resizable vectors, on the other
hand, are more like arrays in Perl or Ruby, lists in Python, or the
`ArrayList` class in Java: they abstract the actual storage, allowing
the vector to grow and shrink as elements are added and removed.

向量是 Common Lisp
基本的整数索引集合，它们分为两大类。定长向量与诸如 Java
这样的语言里的数组非常相似：一块数据头以及一段保存向量元素的连续内存区域。 另一方面，变长向量更像是
Perl 或 Ruby 中的数组，Python
中的列表以及 Java 中的 `ArrayList`
类：它们抽象了实际存储，允许向量随着元素的增加和移除而增大和减小。

You can make fixed-size vectors containing specific values with the
function **VECTOR**, which takes any number of arguments and returns a
freshly allocated fixed-size vector containing those arguments.

你可以用函数 **VECTOR**
来生成含有特定值的定长向量，该函数接受任意数量的参数并返回一个新分配的含有那些参数的定长向量。

```lisp
(vector)     ==> #()
(vector 1)   ==> #(1)
(vector 1 2) ==> #(1 2)
```

The `#(...)` syntax is the literal notation for vectors used by the Lisp
printer and reader. This syntax allows you to save and restore vectors
by **PRINT**ing them out and **READ**ing them back in. You can use the `#(...)`
syntax to include literal vectors in your code, but as the effects of
modifying literal objects aren't defined, you should always use **VECTOR**
or the more general function **MAKE-ARRAY** to create vectors you plan to
modify.

语法 `#(...)` 是 `Lisp`
打印器和读取器使用的向量的字面表示形式，该语法可使你通过用 **PRINT**
打印并用 **READ** 读取以此来保存并恢复向量。可以使用 `#(...)`
语法从而在代码中添加字面向量，但修改字面对象的后果并不明确，因此应当总是使用
**VECTOR** 或更为通用的函数 **MAKE-ARRAY** 来创建打算修改的向量。

**MAKE-ARRAY** is more general than **VECTOR** since you can use it to create
arrays of any dimensionality as well as both fixed-size and resizable
vectors. The one required argument to **MAKE-ARRAY** is a list containing
the dimensions of the array. Since a vector is a one-dimensional
array, this list will contain one number, the size of the vector. As a
convenience, **MAKE-ARRAY** will also accept a plain number in the place
of a one-item list. With no other arguments, **MAKE-ARRAY** will create a
vector with uninitialized elements that must be set before they can be
accessed. To create a vector with the elements all set to a particular
value, you can pass an :initial-element argument. Thus, to make a
five-element vector with its elements initialized to **NIL**, you can
write the following:

**MAKE-ARRAY** 比 **VECTOR**
更加通用，因为你可以用它来创建任何维度的数组以及定长和变长向量。**MAKE-ARRAY**
一个必要参数是一个含有数组维数的列表。由于向量是一维数组，该列表将含有一个数字，也就是向量的大小。出于方便的考量，**MAKE-ARRAY**
也会用一个简单的数字来代替只含有一项的列表。如果没有其他参数，**MAKE-ARRAY**
就将创建一个带有未初始化元素的向量，它们必须在被访问之前设置其值。 为了创建所有元素都设置到一个特定值上的向量，你可以传递一个
`:initial-element` 参数。因此，为了生成一个元素初始化到 **NIL**
的五元素向量，你可以写成下面这样：

```lisp
(make-array 5 :initial-element nil) ==> #(NIL NIL NIL NIL NIL)
```

**MAKE-ARRAY** is also the function to use to make a resizable vector. A
resizable vector is a slightly more complicated object than a
fixed-size vector; in addition to keeping track of the memory used to
hold the elements and the number of slots available, a resizable
vector also keeps track of the number of elements actually stored in
the vector. This number is stored in the vector's fill pointer, so
called because it's the index of the next position to be filled when
you add an element to the vector.

**MAKE-ARRAY**
也是用来创建变长向量的函数。变长向量是比定长向量稍微更复杂的向量。除了跟踪其用来保存元素的内存和可访问的槽位数量，变长向量还要跟踪实际存储在向量中的元素数量。这个数字存放在向量的填充指针里，这样称呼是因为是当你为向量添加一个元素时下一个被填充位置的索引。

To make a vector with a fill pointer, you pass **MAKE-ARRAY** a
`:fill-pointer` argument. For instance, the following call to **MAKE-ARRAY**
makes a vector with room for five elements; but it looks empty because
the fill pointer is zero:

为了创建带有填充指针的向量，你可以向 **MAKE-ARRAY** 传递一个 `:fill-pointer`
实参。例如，下面的 **MAKE-ARRAY**
调用生成了一个带有五元素空间的向量，它看起来是空的因为填充指针是零：

```lisp
(make-array 5 :fill-pointer 0) ==> #()
```

To add an element to the end of a resizable vector, you can use the
function **VECTOR-PUSH**. It adds the element at the current value of the
fill pointer and then increments the fill pointer by one, returning
the index where the new element was added. The function **VECTOR-POP**
returns the most recently pushed item, decrementing the fill pointer
in the process.

为了向可变向量的尾部添加一个元素，你可以使用函数
**VECTOR-PUSH**。它在填充指针的当前值上添加一个元素并将填充指针递增一次，并返回新元素被添加位置的索引。函数
**VECTOR-POP** 返回最近推入的项，并在该过程中递减填充指针。

```lisp
(defparameter *x* (make-array 5 :fill-pointer 0))

(vector-push 'a *x*) ==> 0
*x*                  ==> #(A)
(vector-push 'b *x*) ==> 1
*x*                  ==> #(A B)
(vector-push 'c *x*) ==> 2
*x*                  ==> #(A B C)
(vector-pop *x*)     ==> C
*x*                  ==> #(A B)
(vector-pop *x*)     ==> B
*x*                  ==> #(A)
(vector-pop *x*)     ==> A
*x*                  ==> #()
```

However, even a vector with a fill pointer isn't completely
resizable. The vector `*x*` can hold at most five elements. To make an
arbitrarily resizable vector, you need to pass **MAKE-ARRAY** another
keyword argument: `:adjustable`.

尽管如此，甚至一个带有填充指针的向量也不是完全变长的。向量 `*x*`
只能保存最多五个元素。为了创建一个可任意变长的向量，你需要向 **MAKE-ARRAY**
传递另外一个关键字参数 `:adjustable`。

```lisp
(make-array 5 :fill-pointer 0 :adjustable t) ==> #()
```

This call makes an adjustable vector whose underlying memory can be
resized as needed. To add elements to an adjustable vector, you use
**VECTOR-PUSH-EXTEND**, which works just like **VECTOR-PUSH** except it will
automatically expand the array if you try to push an element onto a
full vector--one whose fill pointer is equal to the size of the
underlying storage.

这个调用生成了一个可调整的向量，其底层内存可以按需调整大小。为了向一个可调整向量添加元素，你可以使用
**VECTOR-PUSH-EXTEND**，它就像 **VECTOR-PUSH**
那样工作，只是在你试图向一个已满的向量（其填充指针等于底层存储的大小）中推入元素时，它能自动扩展该数组。

