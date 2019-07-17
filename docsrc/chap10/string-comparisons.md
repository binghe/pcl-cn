# String Comparisons（字符串比较）

You can compare strings using a set of functions that follow the same
naming convention as the character comparison functions except with
`STRING` as the prefix rather than `CHAR` (see Table 10-3).

你可以使用一组遵循了和字符比较函数相同的命名约定的函数来比较字符串，只不过要将前缀的
`CHAR` 换成 `STRING`（见表 10-3）。

> Table 10-3. String Comparison Functions

| Numeric Analog | Case-Sensitive  | Case-Insensitive        |
| :------------- | :-------------- | :---------------------- |
| `=`	         | **STRING=**	   | **STRING-EQUAL**        |
| `/=`	         | **STRING/=**    | **STRING-NOT-EQUAL**    |
| `<`            | **STRING<**     | **STRING-LESSP**        |
| `>`            | **STRING>**     | **STRING-GREATERP**     |
| `<=`           | **STRING<=**    | **STRING-NOT-GREATERP** |
| `>=`	         | **STRING>=**    | **STRING-NOT-LESSP**    |

However, unlike the character and number comparators, the string
comparators can compare only two strings. That's because they also
take keyword arguments that allow you to restrict the comparison to a
substring of either or both strings. The arguments--`:start1`, `:end1`,
`:start2`, and `:end2`--specify the starting (inclusive) and ending
(exclusive) indices of substrings in the first and second string
arguments. Thus, the following:

但跟字符和数字比较符不同的是，字符串比较符只能比较两个字符串。这是因为它们还带有关键字参数，从而允许你将比较限制在每个或两个字符串的子字符串上。这些参数
`:start1`、`:end1`、`:start2` 和 `:end2`
指定了起始和结束参数字符串中子字符串的起始和终止位置（左闭右开区间）。因此请看下面这个表达式。

```lisp
(string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)
```

..compares the substring "bar" in the two arguments and returns
true. The `:end1` and `:end2` arguments can be **NIL** (or the keyword
argument omitted altogether) to indicate that the corresponding
substring extends to the end of the string.

..在两个参数中比较子字符串 "bar" 并返回真。参数 `:end1` 和 `:end2` 可以为
**NIL**（或者整个关键字参数被省略）指示相应的子字符串扩展到字符串的结尾。

The comparators that return true when their arguments differ--that is,
all of them except **STRING=** and **STRING-EQUAL**--return the index in the
first string where the mismatch was detected.

那些当被比较的两个参数不同时返回真的比较符，也就是除 **STRING=** 和 **STRING-EQUAL**
之外的其他所有操作符，将返回第一个字符串中首次检测到不匹配的索引。

```lisp
(string/= "lisp" "lissome") ==> 3
```

If the first string is a prefix of the second, the return value will
be the length of the first string, that is, one greater than the
largest valid index into the string.

如果第一个字符串是第二个字符串的前缀，返回值是第一个字符串的长度，也就是一个大于字符串中最大有效索引的值。

```lisp
(string< "lisp" "lisper") ==> 4
```

When comparing substrings, the resulting value is still an index into
the string as a whole. For instance, the following compares the
substrings "bar" and "baz" but returns 5 because that's the index of
the _r_ in the first string:

当比较子字符串时，返回值仍然是该字符串作为整体的索引，例如，下面的调用比较子字符串
"bar" 和 "baz" 但却返回了 5，因为它是 _r_ 在第一个字符串中的索引：

```lisp
(string< "foobar" "abaz" :start1 3 :start2 1) ==> 5   ; N.B. not 2
```

Other string functions allow you to convert the case of strings and
trim characters from one or both ends of a string. And, as I mentioned
previously, since strings are really a kind of sequence, all the
sequence functions I'll discuss in the next chapter can be used with
strings. For instance, you can discover the length of a string with
the **LENGTH** function and can get and set individual characters of a
string with the generic sequence element accessor function, **ELT**, or
the generic array element accessor function, **AREF**. Or you can use the
string-specific accessor, **CHAR**. But those functions, and others, are
the topic of the next chapter, so let's move on.

其他字符串函数允许你转化字符串的大小写以及从一个字符串的一端或两端修剪字符。并且如同我前面提到的，由于字符串实际上是一种序列，我将在下一章所讨论的所有序列函数都可用于字符串。例如，你可以用
**LENGTH**
函数来检查字符串的长度并获取和设定字符串中的个别字符，使用通用序列元素访问函数
**ELT** 或者使用通用数组元素访问函数
**AREF**。你还可以使用特定于字符串的访问函数
**CHAR**。这些以及其他一些函数都是下一章的主题，那么让我们继续介绍。
