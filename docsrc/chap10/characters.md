# Characters（字符）

Common Lisp characters are a distinct type of object from
numbers. That's as it should be--characters are not numbers, and
languages that treat them as if they are tend to run into problems
when character encodings change, say, from 8-bit ASCII to 21-bit
Unicode.11 Because the Common Lisp standard didn't mandate a
particular representation for characters, today several Lisp
implementations use Unicode as their "native" character encoding
despite Unicode being only a gleam in a standards body's eye at the
time Common Lisp's own standardization was being wrapped up.

Common Lisp
字符和数字是不同类型的对象。其本该如此——字符不是数字，而将其同等对待的语言当字符编码改变时（比如说从
8 位 ASCII 到 21 位 Unicode）可能会出现问题。由于 Common Lisp
标准并未规定字符的内部表示方法，当今几种 Lisp 实现都使用 Unicode
作为其原生字符编码，尽管从标准化组织的观点来看 Unicode 在 Common
Lisp 自身的标准化成型时期只是昙花一现。

The read syntax for characters objects is simple: `#\` followed by the
desired character. Thus, `#\x` is the character `x`. Any character can be
used after the `#\`, including otherwise special characters such as `"`,
`(`, and whitespace. However, writing whitespace characters this way
isn't very (human) readable; an alternative syntax for certain
characters is `#\` followed by the character's name. Exactly what names
are supported depends on the character set and on the Lisp
implementation, but all implementations support the names `Space` and
`Newline`. Thus, you should write `#\Space` instead of `#\ `, though the
latter is technically legal. Other semistandard names (that
implementations must use if the character set has the appropriate
characters) are `Tab`, `Page`, `Rubout`, `Linefeed`, `Return`, and `Backspace`.

字符的读取语法很简单：`#\` 后跟想要的字符。这样，`#\x` 就是字符
`x`。任何字符都可以用在 `#\` 之后，包括那些诸如 `"`、`(`
和空格这样的特殊字符。但以这种方式来写空格字符却并不是十分的人类可读，特定字符的替代语法是
`#\` 后跟该字符的名字。具体支持的名字取决于字符集和所在的
Lisp 实现，但所有实现都支持名字 `Space` 和
`Newline`。这样就应该写成用 `#\Space` 来代替
`#\`，尽管后者在技术上是合法的。其他半标准化的名字（如果字符集包含相应的字符实现就必须采用的名字）是
`Tab`、`Page`、`Rubout`、`Linefeed`、`Return` 和 `Backspace`。
