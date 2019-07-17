# Strings（字符串）

As mentioned earlier, strings in Common Lisp are really a composite
data type, namely, a one-dimensional array of
characters. Consequently, I'll cover many of the things you can do
with strings in the next chapter when I discuss the many functions for
manipulating sequences, of which strings are just one type. But
strings also have their own literal syntax and a library of functions
for performing string-specific operations. I'll discuss these aspects
of strings in this chapter and leave the others for Chapter 11.

如前所述，Common Lisp
中的字符串其实是一种复合数据类型，也即一个一维字符数组。因此，我将在下一章讨论用来处理序列的许多函数时将谈及许多字符串应用，因为字符串只不过是一种序列。但字符串也有其自己的字面语法和一个用来进行字符串特定操作的函数数库。本章将讨论字符串的这些方面并将其余部分留到第
11 章再作介绍。

As you've seen, literal strings are written enclosed in double
quotes. You can include any character supported by the character set
in a literal string except double quote (`"`) and backslash (`\`). And you
can include these two as well if you escape them with a backslash. In
fact, backslash always escapes the next character, whatever it is,
though this isn't necessary for any character except for `"` and
itself. Table 10-2 shows how various literal strings will be read by
the Lisp reader.

正如你所看到的那样，字面字符串写在闭合的双引号里。你可以在一个字面字符串中包括任何字符集支持的字符，除了双引号（`"`）和反斜杠（`\`）。而如果你将它们用一个反斜杠转义的话也可以包括这两个字符。事实上，反斜杠总是转义其下一个字符，无论它是什么，尽管这对于除了 `"`
和它本身之外的其他字符并不必要。表 10-2
显示了不同的字面字符串是如何被 Lisp 读取器读取的。

> Table 10-2. Literal Strings

| Literal        | Contents | Comment                                       |
| :------------- | :------- | :-------------------------------------------- |
| `"foobar"`     | foobar   | Plain string.                                 |
| `"foo\"bar"`	 | foo"bar	| The backslash escapes quote.                  |
| `"foo\\bar"`	 | foo\bar	| The first backslash escapes second backslash. |
| `"\"foobar\""` | "foobar"	| The backslashes escape quotes.                |
| `"foo\bar"`    | foobar	| The backslash "escapes" b                     |

Note that the REPL will ordinarily print strings in readable form,
adding the enclosing quotation marks and any necessary escaping
backslashes, so if you want to see the actual contents of a string,
you need to use function such as **FORMAT** designed to print
human-readable output. For example, here's what you see if you type a
string containing an embedded quotation mark at the REPL:

注意，REPL
将以可读的形式原样打印字符串，并带有外围的引号和任何必要的转义反斜杠。因此，如果想要看到一个字符串的实际内容，就要使用诸如FORMAT这种原本就是设计用于打印出可读性良好输出的函数。例如，下面是在
REPL 中输入一个含有内嵌引号的字符串时所看到的输出：

```lisp
CL-USER> "foo\"bar"
"foo\"bar"
```

**FORMAT**, on the other hand, will show you the actual string contents:

另一方面，**FORMAT** 将显示出实际的字符串内容：

```lisp
CL-USER> (format t "foo\"bar")
foo"bar
NIL
```

