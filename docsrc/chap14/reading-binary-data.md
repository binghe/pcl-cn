# Reading Binary Data（读取二进制数据）

By default **OPEN** returns character streams, which translate the
underlying bytes to characters according to a particular
character-encoding scheme. To read the raw bytes, you need to pass
**OPEN** an `:element-type` argument of `'(unsigned-byte 8)`. You can pass
the resulting stream to the function **READ-BYTE**, which will return an
integer between 0 and 255 each time it's called. **READ-BYTE**, like the
character-reading functions, also accepts optional arguments to
specify whether it should signal an error if called at the end of the
file and what value to return if not. In Chapter 24 you'll build a
library that allows you to conveniently read structured binary data
using **READ-BYTE**.

默认情况下，**OPEN**
返回字符流，它根据特定的字符编码方案将底层字节转化成字符。 为了读取原
始字节，你需要向
**OPEN** 传递一个值为 `'(unsigned-byte 8)` 的 `:element-type`
参数。 你可以将得到的流传给
**READ-BYTE**，它将在每次被调用时返回一个从 0 到 255 之间的整数。与字符读取函数一样，**READ-BYTE**
也支持可选的参数此便指定当其被调用在文件结尾时是否应该报错，以及在遇到结尾时返回何值。在第
24 章里你将构建一个库，它允许使用 **READ-BYTE**
来便利地读取结构化的二进制数据。

