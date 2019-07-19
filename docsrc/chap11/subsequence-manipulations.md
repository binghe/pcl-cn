# Subsequence Manipulations（操纵子序列）

Another set of functions allows you to manipulate subsequences of
existing sequences. The most basic of these is **SUBSEQ**, which extracts
a subsequence starting at a particular index and continuing to a
particular ending index or the end of the sequence. For instance:

另一类函数允许你对已有序列的子序列进行操作。其中最基本的是
**SUBSEQ**，其解出序列中从一个特定索引开始并延续到一个特定终止索引或结尾处的子序列。例如：

```lisp
(subseq "foobarbaz" 3)   ==> "barbaz"
(subseq "foobarbaz" 3 6) ==> "bar"
```

**SUBSEQ** is also **SETF**able, but it won't extend or shrink a
sequence; if the new value and the subsequence to be replaced are
different lengths, the shorter of the two determines how many
characters are actually changed.

**SUBSEQ** 也支持
**SETF**，但不会扩大或缩小一个序列。如果新的值和将被替换的子序列具有不同的长度，那么两者中较短的那一个将决定有多少个字符被实际改变。

```lisp
(defparameter *x* (copy-seq "foobarbaz"))

(setf (subseq *x* 3 6) "xxx")  ; subsequence and new value are same length
*x* ==> "fooxxxbaz"

(setf (subseq *x* 3 6) "abcd") ; new value too long, extra character ignored.
*x* ==> "fooabcbaz"

(setf (subseq *x* 3 6) "xx")   ; new value too short, only two characters changed
*x* ==> "fooxxcbaz"
```

You can use the **FILL** function to set multiple elements of a
sequence to a single value. The required arguments are a sequence and
the value with which to fill it. By default every element of the
sequence is set to the value; `:start` and `:end` keyword arguments can
limit the effects to a given subsequence.

你可以使用 **FILL**
函数来将一个序列的多个元素设置到单个值上。所需的参数是一个序列以及所填充的值。在默认情况下，该序列的每个元素都被设置到该值上。`:start`
和 `:end` 关键字参数可以将效果限制在一个给定的子序列上。

If you need to find a subsequence within a sequence, the **SEARCH**
function works like **POSITION** except the first argument is a sequence
rather than a single item.

如果你需要在一个序列中查找一个子序列，**SEARCH**
函数可以像 **POSITION** 那样工作，不过第一个参数是一个序列而不是一个单独的项。

```lisp
(position #\b "foobarbaz") ==> 3
(search "bar" "foobarbaz") ==> 3
```

On the other hand, to find where two sequences with a common prefix
first diverge, you can use the **MISMATCH** function. It takes two
sequences and returns the index of the first pair of mismatched
elements.

另一方面，为了找出两个带有相同前缀的序列首次分岔的位置，你可以使用
**MISMATCH** 函数。它接受两个序列并返回第一对不相匹配的元素的索引。

```lisp
(mismatch "foobarbaz" "foom") ==> 3
```

It returns **NIL** if the strings match. **MISMATCH** also takes many of the
standard keyword arguments: a `:key` argument for specifying a function
to use to extract the values to be compared; a `:test` argument to
specify the comparison function; and `:start1`, `:end1`, `:start2`, and
`:end2` arguments to specify subsequences within the two sequences. And
a `:from-end` argument of **T** specifies the sequences should be searched
in reverse order, causing **MISMATCH** to return the index, in the first
sequence, where whatever common suffix the two sequences share begins.

如果字符串匹配，它将返回 **NIL**。**MISMATCH**
也接受许多标准关键字参数：`:key`
参数可以指定一个函数用来抽取出被比较的值；`:test`
参数用于指定比较函数；而 `:start1`、`:end1`、`:start2` 和 `:end2`
参数可以指定两个序列中的子序列。另外，一个设置为 **T**
的 `:from-end`
参数可以指定以相反的顺序搜索序列，从而导致 **MISMATCH**
返回两个序列的相同后缀在第一个序列中开始位置的索引。

```lisp
(mismatch "foobar" "bar" :from-end t) ==> 3
```
