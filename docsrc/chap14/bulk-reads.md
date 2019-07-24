# Bulk Reads（批量读取）

One last reading function, **READ-SEQUENCE**, works with both character
and binary streams. You pass it a sequence (typically a vector) and a
stream, and it attempts to fill the sequence with data from the
stream. It returns the index of the first element of the sequence that
wasn't filled or the length of the sequence if it was able to
completely fill it. You can also pass `:start` and `:end` keyword
arguments to specify a subsequence that should be filled instead. The
sequence argument must be a type that can hold elements of the
stream's element type. Since most operating systems support some form
of block I/O, **READ-SEQUENCE** is likely to be quite a bit more efficient
than filling a sequence by repeatedly calling **READ-BYTE** or **READ-CHAR**.

最后一个读取函数 **READ-SEQUENCE**
可同时工作在字符和二进制流上。你传递给它一个序列（通常是一个向量）和一个流，然后它会尝试用来自流的数据填充该序列。它返回序列中第一个没有被填充的元素的索引，或是在完全填充的情况下返回该序列的长度。你也可以传递
`:start` 和 `:end`
关键字参数来指定一个应当被代替填充的子序列。该序列参数的元素类型必须足
以保存带有该流元素类型的元素。由于多数操作系统支持某种形式的块
I/O，**READ-SEQUENCE** 通常比重复调用
**READ-BYTE** 或 **READ-CHAR** 来填充一个序列更加高效。
