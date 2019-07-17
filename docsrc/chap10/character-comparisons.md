# Character Comparisons（字符比较）

The main thing you can do with characters, other than putting them
into strings (which I'll get to later in this chapter), is to compare
them with other characters. Since characters aren't numbers, you can't
use the numeric comparison functions, such as `<` and `>`. Instead, two
sets of functions provide character-specific analogs to the numeric
comparators; one set is case-sensitive and the other case-insensitive.

可以对字符做的主要操作除了将它们放进字符串（我将在本章后面讨论这点）之外，还可将它们与其他字符相比较。由于字符不是数字，所以不能使用诸如
`<` 和`>`
这样的数值比较函数。作用替代，有两类函数提供了数值比较符的特定于字符的相似物：一类是大小写相关的，而另一类是大小写无关的。

The case-sensitive analog to the numeric `=` is the function **CHAR=**. Like
`=`, **CHAR=** can take any number of arguments and returns true only if
they're all the same character. The case- insensitive version is
**CHAR-EQUAL**.

数值 `=` 的大小写相关相似物是函数 **CHAR=**。像
`=` 那样，**CHAR=**
可以接受任意数量的实参并只在它们全是相同字符时才返回真。大小写无关版本是
**CHAR-EQUAL**。

The rest of the character comparators follow this same naming scheme:
the case-sensitive comparators are named by prepending the analogous
numeric comparator with CHAR; the case-insensitive versions spell out
the comparator name, separated from the **CHAR** with a hyphen. Note,
however, that `<=` and `>=` are "spelled out" with the logical equivalents
**NOT-GREATERP** and **NOT-LESSP** rather than the more verbose
**LESSP-OR-EQUALP** and **GREATERP-OR-EQUALP**. Like their numeric
counterparts, all these functions can take one or more
arguments. Table 10-1 summarizes the relation between the numeric and
character comparison functions.

其余的字符比较函数遵循了相同的命名模式：大小写相关的比较符通过在其对应的数值比较符前面加上
**CHAR** 来命名：大小写无关的版本拼出比较符的名字，前面加上 **CHAR**
和一个连字符。不过，注意 `<=` 和 `>=` 被拼写成其逻辑等价形式
**NOT-GREATERP** 和 **NOT-LESSP**，而不是更确切的 **LESSP-OR-EQUALP**
和 **GREATERP-OR-EQUALP**。和它们的数值等价物一样，所有这些函数都接受一个或更多参数。表
10-1 总结了数值和字符比较函数之间的关系。

> Table 10-1. Character Comparison Functions

| Numeric Analog | Case-Sensitive | Case-Insensitive      |
| :------------- | :------------- | :-------------------- |
| `=`            | **CHAR=**      | **CHAR-EQUAL**        |
| `/=`           | **CHAR/=**     | **CHAR-NOT-EQUAL**    |
| `<`            | **CHAR<**      | **CHAR-LESSP**        |
| `>`            | **CHAR>**      | **CHAR-GREATERP**     |
| `<=`           | **CHAR<=**     | **CHAR-NOT-GREATERP** |
| `>=`           | **CHAR>=**     | **CHAR-NOT-LESSP**    |


Other functions that deal with characters provide functions for, among
other things, testing whether a given character is alphabetic or a
digit character, testing the case of a character, obtaining a
corresponding character in a different case, and translating between
numeric values representing character codes and actual character
objects. Again, for complete details, see your favorite Common Lisp
reference.

其他处理字符的函数包括测试一个给定字符是否是字母或者数字字符，测试一个字符的大小写，获取不同大小写的对应字符，以及在代表字符编码的数值和实际字符对象之间转化。对于完整的细节，请参见你喜爱的
Common Lisp 参考。
