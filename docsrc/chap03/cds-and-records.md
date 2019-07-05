# CDs and Records（CD 和记录）

To keep track of CDs that need to be ripped to MP3s and which CDs
should be ripped first, each record in the database will contain the
title and artist of the CD, a rating of how much the user likes it,
and a flag saying whether it has been ripped. So, to start with,
you'll need a way to represent a single database record (in other
words, one CD). Common Lisp gives you lots of choices of data
structures from a simple four-item list to a user-defined class, using
the Common Lisp Object System (CLOS).

为了记录那些需要转换成 MP3 的 CD，以及哪些 CD
应该先进行转换，数据库里的每条记录都将包含 CD
的标题和艺术家信息，一个关于有多少用户喜欢它的评级，以及一个表示其是否已经被转换过的标记。因此，首先需要一种方式来表示一条单一的数据库记录（也就是一张
CD）。Common Lisp 提供了大量的数据结构可供选择——从简单的四元素列表到基于
Common Lisp 对象系统（CLOS）的用户自定义类。

For now you can stay at the simple end of the spectrum and use a
list. You can make a list with the **LIST** function, which, appropriately
enough, returns a list of its arguments.

眼下你只能选择该系列里最简单的方法－使用列表。你可以使用 **LIST**
函数来生成一个列表，如果正常执行的话，它将返回一个由其参数所组成的列表。

```
CL-USER> (list 1 2 3)
(1 2 3)
```

You could use a four-item list, mapping a given position in the list
to a given field in the record. However, another flavor of
list--called a property list, or plist for short--is even more
convenient. A plist is a list where every other element, starting with
the first, is a symbol that describes what the next element in the
list is. I won't get into all the details of exactly what a symbol is
right now; basically it's a name. For the symbols that name the fields
in the CD database, you can use a particular kind of symbol, called a
keyword symbol. A keyword is any name that starts with a colon (`:`),
for instance, `:foo`. Here's an example of a plist using the keyword
symbols `:a`, `:b`, and `:c` as property names:

还可以使用一个四元素列表，将列表中的给定位置映射到记录中的给定字段。然
而，使用另一类被称为属性表（property list）或简称 plist
的列表甚至更方便。属性表是这样一种列表：从第一个元素开始的所有相间元素都是一个用来描述接下来的那个元素的符号。目前我不会深入讨论关于符号的所有细节，基本上它就是一个名字。对于用来命名
CD 数据库字段的名字，你可以使用一种特殊类型的符号——关键字（keyword）符号。关键字符号是任何以冒号开始的名字，例如
`:foo`。下面是一个使用了关键字符号 `:a`、`:b` 和 `:c`作为属性名的示例 plist：

```
CL-USER> (list :a 1 :b 2 :c 3)
(:A 1 :B 2 :C 3)
```

Note that you can create a property list with the same **LIST** function
as you use to create other lists; it's the contents that make it a
plist.

注意，你可以使用和创建其他列表时同样的 **LIST**
函数来创建一个属性表，只是特殊的内容使其成为了属性表。

The thing that makes plists a convenient way to represent the records
in a database is the function **GETF**, which takes a plist and a symbol
and returns the value in the plist following the symbol, making a
plist a sort of poor man's hash table. Lisp has real hash tables too,
but plists are sufficient for your needs here and can more easily be
saved to a file, which will come in handy later.

真正令属性表便于表达数据库记录的原则是在于函数 **GETF**，其接受一个
plist 和一个符号，并返回 plist 中跟在那个符号后面的值，这使得 plist
成为了穷人的哈希表。当然 Lisp 有真正的哈希表，但 plist
足以满足当前需要，并且可以更容易地保存在文件里——后面将谈及这点。

```
CL-USER> (getf (list :a 1 :b 2 :c 3) :a)
1
CL-USER> (getf (list :a 1 :b 2 :c 3) :c)
3
```

Given all that, you can easily enough write a function `make-cd` that
will take the four fields as arguments and return a plist representing
that CD.

理解了所有这些知识，你就可以轻易写出一个 `make-cd` 函数了，它以参数的形式接受
4 个字段，然后返回一个代表该 CD 的 plist。

```
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
```
  
The word **DEFUN** tells us that this form is defining a new function. The
name of the function is `make-cd`. After the name comes the parameter
list. This function has four parameters: `title`, `artist`, `rating`, and
`ripped`. Everything after the parameter list is the body of the
function. In this case the body is just one form, a call to
**LIST**. When `make-cd` is called, the arguments passed to the call will
be bound to the variables in the parameter list. For instance, to
make a record for the CD Roses by Kathy Mattea, you might call
`make-cd` like this:

单词 **DEFUN** 告诉我们上述形式正在定义一个新函数，函数名是
`make-cd`。跟在名字后面的是形参列表，这个函数拥有四个形参：`title`、`artist`、`rating`
和 `ripped`。形参列表后面的都是函数体。本例中的函数体只有一个形式，即对
**LIST** 的调用。当 `make-cd`
被调用时，传递给该调用的参数将被绑定到形参列表中的变量上。例如，为了建立一个关于
Kathy Mattea 的名为 Roses 的 CD 的记录，你可以这样调用 `make-cd`：

```
CL-USER> (make-cd "Roses" "Kathy Mattea" 7 t)
(:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T)
```

