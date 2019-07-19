# Sequence Mapping Functions（序列映射函数）

Finally, the last of the sequence functions are the generic mapping
functions. **MAP**, like the sequence predicate functions, takes a
n-argument function and n sequences. But instead of a boolean value,
**MAP** returns a new sequence containing the result of applying the
function to subsequent elements of the sequences. Like **CONCATENATE** and
**MERGE**, **MAP** needs to be told what kind of sequence to create.

最终，序列函数的最后是通用映射函数。**MAP**
和序列谓词函数一样，接受一个带有 n
个参数函数和 n 个序列。但并非返回布尔值，**MAP**
返回一个新序列，它由那些将函数应用在序列的相继元素上所得到的结果组成。与
**CONCATENATE** 和 **MERGE** 相似，**MAP** 需要被告知其所创建序列的类型。

```lisp
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) ==> #(10 18 24 28 30)
```

**MAP-INTO** is like **MAP** except instead of producing a new
sequence of a given type, it places the results into a sequence
passed as the first argument. This sequence can be the same as one
of the sequences providing values for the function. For instance, to
sum several vectors--`a`, `b`, and `c`--into one, you could write this:

**MAP-INTO** 和 **MAP**
相似，但它并不产生给定类型的新序列，而是将结果放置在一个作为第一个参数传递的序列中。这个序列可以和为函数提供值的序列中的一个相同。例如，为了将几个向量
`a`、`b` 和 `c` 相加到其中一个向量里，你可以写成这样：

```lisp
(map-into a #'+ a b c)
```

If the sequences are different lengths, **MAP-INTO** affects only as many
elements as are present in the shortest sequence, including the
sequence being mapped into. However, if the sequence being mapped into
is a vector with a fill pointer, the number of elements affected isn't
limited by the fill pointer but rather by the actual size of the
vector. After a call to **MAP-INTO**, the fill pointer will be set to the
number of elements mapped. **MAP-INTO** won't, however, extend an
adjustable vector.

如果这些序列的长度不同，那么 **MAP-INTO**
将只影响那些与最短序列元素数量相当元素，其中也包括那个将被映射到的序列。不过，如果序列被映射到一个带有填充指针的向量里，受影响元素的数量将不限于填充指针而是该向量的实际大小。在一个对
**MAP-INTO**
的调用之后，填充指针将被设置成被映射元素的数量。尽管如此，**MAP-INTO**
将不会扩展一个可调整大小的向量。

The last sequence function is **REDUCE**, which does another kind of
mapping: it maps over a single sequence, applying a two-argument
function first to the first two elements of the sequence and then to
the value returned by the function and subsequent elements of the
sequence. Thus, the following expression sums the numbers from one to
ten:

最后一个序列函数是
**REDUCE**，它可以做另一种类型的映射：映射在单个序列上，先将一个两参数函数应用到序列的最初两个元素上，再将函数返回的值和序列的后续元素继续用于该函数。这样，下面的表达式将对从
1 到 10 的整数求和：

```lisp
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10)) ==> 55
```

**REDUCE** is a surprisingly useful function--whenever you need to
distill a sequence down to a single value, chances are you can write
it with **REDUCE**, and it will often be quite a concise way to express
what you want. For instance, to find the maximum value in a sequence
of numbers, you can write `(reduce #'max numbers)`. **REDUCE** also takes
a full complement of keyword arguments (`:key`, `:from-end`, `:start`, and
`:end`) and one unique to **REDUCE** (`:initial-value`). The latter
specifies a value that's logically placed before the first element
of the sequence (or after the last if you also specify a true
`:from-end` argument).
  
**REDUCE**
函数非常有用，无论何时，当需要将一个序列提炼成一个单独的值时，你都有机会用
**REDUCE**
来写它，而这通常都是一种相当简洁的表达意图的方法。例如，为了找出一个数字序列中的最大值，你可以写成
`(reduce #'max numbers)`。**REDUCE**
也接受完整的关键字参数（`:key`、`:from-end`、`:start` 和 `:end`）以及一个
**REDUCE** 专用的
`:intial-value`。后者可以指定一个值，在逻辑上被放置在序列的第一个元素之前（或是如果你同时指定了一个为真的
`:from-end` 参数，那么该值被放置在序列的最后一个元素之后）。
