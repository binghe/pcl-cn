# Querying the Database（查询数据库）

Now that you have a way to save and reload the database to go along
with a convenient user interface for adding new records, you soon may
have enough records that you won't want to be dumping out the whole
database just to look at what's in it. What you need is a way to query
the database. You might like, for instance, to be able to write
something like this:

有了保存和重载数据库的方法，并且可以用一个便利的用户接口来添加新记录，
很快就会出现足够多的记录，但你并不想为了查看它里面有什么而每次都把整个
数据库导出来。比如说，你可能希望能够通过类似下面的查询


```lisp
(select :artist "Dixie Chicks")
```

and get a list of all the records where the artist is the Dixie
Chicks. Again, it turns out that the choice of saving the records in a
list will pay off.

来获得艺术家 Dixie Chicks 的所有记录的列表。这又证明了当初选择用列表来保存记录是明智的。

The function **REMOVE-IF-NOT** takes a predicate and a list and returns a
list containing only the elements of the original list that match the
predicate. In other words, it has removed all the elements that don't
match the predicate. However, **REMOVE-IF-NOT** doesn't really remove
anything--it creates a new list, leaving the original list
untouched. It's like running grep over a file. The predicate argument
can be any function that accepts a single argument and returns a
boolean value--**NIL** for false and anything else for true.

函数 **REMOVE-IF-NOT**
接受一个谓词和一个原始列表，然后返回一个仅包含原始列表中匹配该谓词的所有元素的新列表。换句话说，它删除了所有不匹配该谓词的元素。然而，**REMOVE-IF-NOT**
并没有真的删除任何东西——它会创建一个新列表，而不会去碰原始列表。这就好比是在一个文件上运行
`grep`。谓词参数可以是任何接受单一参数并能返回布尔值的函数——除了
**NIL** 代表假以外其余的都代表真。

For instance, if you wanted to extract all the even elements from a
list of numbers, you could use **REMOVE-IF-NOT** as follows:

举个例子，假如要从一个由数字组成的列表里抽出所有偶数来，就可以像下面这样来使用
**REMOVE-IF-NOT**：

```lisp
CL-USER> (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
```

In this case, the predicate is the function **EVENP**, which returns true
if its argument is an even number. The funny notation `#'` is shorthand
for "Get me the function with the following name." Without the #',
Lisp would treat evenp as the name of a variable and look up the value
of the variable, not the function.

这里的谓词是函数 **EVENP**，当其参数是偶数时返回真。那个有趣的 `#'`
记号是 “获取函数，其名如下” 的简称。没有 `#'`
的话，Lisp 将把 `evenp` 作为一个变量名来对待并查找该变量的值，而不是将其看作函数。

You can also pass **REMOVE-IF-NOT** an anonymous function. For instance,
if **EVENP** didn't exist, you could write the previous expression as the
following:

你也可以向 **REMOVE-IF-NOT** 传递一个匿名函数。例如，如果 **EVENP**
不存在，你也可以像下面这样来写前面给出的表达式：

```lisp
CL-USER> (remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
```

In this case, the predicate is this anonymous function:

在这种情况下，谓词是下面这个匿名函数：

```lisp
(lambda (x) (= 0 (mod x 2)))
```

which checks that its argument is equal to 0 modulus 2 (in other
words, is even). If you wanted to extract only the odd numbers using
an anonymous function, you'd write this:

它会检查其实参与 2 取模时等于 0
的情况（换句话说，就是偶数）。如果想要用匿名函数来抽出所有的奇数，就可以这样写：

```lisp
CL-USER> (remove-if-not #'(lambda (x) (= 1 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(1 3 5 7 9)
```

Note that `lambda` isn't the name of the function--it's the indicator
you're defining an anonymous function.5 Other than the lack of a name,
however, a **LAMBDA** expression looks a lot like a **DEFUN**: the word lambda
is followed by a parameter list, which is followed by the body of the
function.

注意，`lambda` 并不是函数的名字——它只是一个表明你正在定义匿名函数的指示器。但除了缺少名字以外，一个
**LAMBDAL** 表达式看起来很像一个 **DEFUN**：单词 `lambda`
后面紧跟着形参列表，然后再是函数体。

To select all the Dixie Chicks' albums in the database using
**REMOVE-IF-NOT**, you need a function that returns true when the artist
field of a record is "Dixie Chicks". Remember that we chose the plist
representation for the database records because the function **GETF** can
extract named fields from a plist. So assuming cd is the name of a
variable holding a single database record, you can use the expression
`(getf cd :artist)` to extract the name of the artist. The function
**EQUAL**, when given string arguments, compares them character by
character. So `(equal (getf cd :artist) "Dixie Chicks")` will test
whether the artist field of a given CD is equal to "Dixie Chicks". All
you need to do is wrap that expression in a **LAMBDA** form to make an
anonymous function and pass it to **REMOVE-IF-NOT**.

为了用 **REMOVE-IF-NOT** 从数据库里选出所有 Dixie Chicks
的专辑，需要可以在一条记录的艺术家字段是 "Dixie Chicks"
时返回真的函数。请记住，我们之所以选择 plist 来表达数据库的记录是因为函数
**GETF** 可以从 plist 里抽出给定名称的字段来。因此假设 `cd`
是保存着数据库单一记录的变量的名字，那么可以使用表达式 `(getf cd :artist)`
来抽出艺术家名字来。当给函数 **EQUAL** 赋予字符串参数时，可以逐个字符地比较它们。因此
`(equal (getf cd :artist) "Dixie Chicks")`
将测试一个给定 CD 的艺术家字段是否等于 "Dixie Chicks"。所需做的只是将这个表达式包装在一个
**LAMBDA** 形式，里从而得到一个匿名函数，然后传递给 **REMOVE-IF-NOT**。

```lisp
CL-USER> (remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
```

Now suppose you want to wrap that whole expression in a function that
takes the name of the artist as an argument. You can write that like
this:

现在假设要将整个表达式包装进一个接受艺术家名字作为参数的函数里，可以写成这样：

```lisp
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))
```

Note how the anonymous function, which contains code that won't run
until it's invoked in **REMOVE-IF-NOT**, can nonetheless refer to the
variable artist. In this case the anonymous function doesn't just
save you from having to write a regular function--it lets you write
a function that derives part of its meaning--the value of
artist--from the context in which it's embedded.
  
这个匿名函数的代码直到其被 **REMOVE-IF-NOT**
调用才会运行，它是如何访问到变量 artist
的。在这种情况下，匿名函数不仅使你免于编写一个正规函数，而且还可以编写出一个其部分含义（artist
值）取自上下文环境的函数。

So that's `select-by-artist`. However, selecting by artist is only one
of the kinds of queries you might like to support. You could write
several more functions, such as `select-by-title`, `select-by-rating`,
`select-by-title-and-artist`, and so on. But they'd all be about the
same except for the contents of the anonymous function. You can
instead make a more general select function that takes a function as
an argument.

以上就是 `select-by-artist`。尽管如此，通过艺术家来搜索只是你想要支持的各种查询方法的一种，还可以编写其他几个函数，诸如
`select-by-title`、`select-by-rating`、`select-by-title-and-artist`，
等等。但它们之间除了匿名函数的内容以外就没有其他区别了。换个做法，可以做出一个更加通用的
`select` 函数来，它接受一个函数作为其实参。

```lisp
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
```

So what happened to the `#'`? Well, in this case you don't want
**REMOVE-IF-NOT** to use the function named `selector-fn`. You want it to
use the anonymous function that was passed as an argument to `select` in
the variable `selector-fn`. Though, the `#'` comes back in the call to
select.

但是 `#'` 到哪里去了？这是因为你并不希望 **REMOVE-IF-NOT**
在此去使用一个名为 `selector-fn` 的函数。它应该使用的是一个作为 `select`
的实参传递到变量 `selector-fn` 里的匿名函数。不过，在对 `select`
的调用中，`#'` 还是会出现。

```lisp
CL-USER> (select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
```

But that's really quite gross-looking. Luckily, you can wrap up the
creation of the anonymous function.

但这样看起来相当粗糙。所幸可以将匿名函数的创建过程包装起来。

```lisp
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
```

This is a function that returns a function and one that references a
variable that--it seems--won't exist after `artist-selector` returns.
It may seem odd now, but it actually works just the way you'd want--if
you call `artist-selector` with an argument of "Dixie Chicks", you get
an anonymous function that matches CDs whose `:artist` field is "Dixie
Chicks", and if you call it with "Lyle Lovett", you get a different
function that will match against an `:artist` field of "Lyle Lovett". So
now you can rewrite the call to select like this:

这是一个返回函数的函数，并且返回的函数里引用了一个似乎在 `artist-selector`
返回以后将不会存在的变量。 尽管它现在可能看起来有些奇怪，但它确实可以按照你所想象的方式来工作——如果用参数
"Dixie Chicks" 调用 `artist-selector`，那么将得到一个可以匹配其 `:artist`
字段为 "Dixie Chicks" 的 CD 的匿名函数，而如果用 "Lyle Lovett"
来调用它，就将得到另一个匹配 `:artist` 字段为 "Lyle Lovett"
的函数。所以现在可以像下面这样来重写前面的 `select` 调用了：

```lisp
CL-USER> (select (artist-selector "Dixie Chicks"))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
```
 
Now you just need some more functions to generate selectors. But just
as you don't want to have to write `select-by-title`, `select-by-rating`,
and so on, because they would all be quite similar, you're not going
to want to write a bunch of nearly identical selector-function
generators, one for each field. Why not write one general-purpose
selector-function generator, a function that, depending on what
arguments you pass it, will generate a selector function for different
fields or maybe even a combination of fields? You can write such a
function, but first you need a crash course in a feature called
keyword parameters.

现在只需要用更多的函数来生成选择器了。但正如不想编写 `select-by-title`，
`select-by-rating`
等雷同的东西那样，你也不会想去写一大堆长相差不多每个字段写一个的选择器函数生成器。那么为什么不写一个通用的选择器函数生成器呢？让它根据传递给它的参数，生成用于不同字段甚至字段组合的选择器函数？完全可以写出这样一个函数来，不过首先需要快速学习一下关键字行参（keyword
parameter）的有关内容。

In the functions you've written so far, you've specified a simple list
of parameters, which are bound to the corresponding arguments in the
call to the function. For instance, the following function:

目前写过的函数使用的都是一个简单的形参列表，随后被绑定到函数调用中对应的实参上。例如，下列函数

```lisp
(defun foo (a b c) (list a b c))
```

has three parameters, `a`, `b`, and `c`, and must be called with three
arguments. But sometimes you may want to write a function that can be
called with varying numbers of arguments. Keyword parameters are one
way to achieve this. A version of `foo` that uses keyword parameters
might look like this:

有三个形参，`a`、`b` 和 `c`，并且必须用三个实参来调用。但有时可能想要
编写一个可以用任何数量的实参来调用的函数，关键字形参就是其中一种实现方
式。使用关键字形参的 `foo` 版本可能看起来是这样的：

```lisp
(defun foo (&key a b c) (list a b c))
```

The only difference is the `&key` at the beginning of the argument
list. However, the calls to this new `foo` will look quite
different. These are all legal calls with the result to the right of
the `==>`:

它与前者唯一的区别在于形参列表的开始处有一个 `&key`。但是，对这个新 `foo`
的调用方法将是截然不同的。下面这些调用都是合法的，同时在 `==>`
的右边给出了相应的结果。

```lisp
(foo :a 1 :b 2 :c 3)  ==> (1 2 3)
(foo :c 3 :b 2 :a 1)  ==> (1 2 3)
(foo :a 1 :c 3)       ==> (1 NIL 3)
(foo)                 ==> (NIL NIL NIL)
```

As these examples show, the value of the variables `a`, `b`, and `c` are
bound to the values that follow the corresponding keyword. And if a
particular keyword isn't present in the call, the corresponding
variable is set to **NIL**. I'm glossing over a bunch of details of how
keyword parameters are specified and how they relate to other kinds of
parameters, but you need to know one more detail.

正如这些示例所显示的，变量 `a`、`b` 和 `c`
的值被绑定到了跟在相应的关键字后面的值上。并且如果一个特定的关键字在调用中没有指定，那么对应的变量将被设置成
**NIL**。关于关键字形参如何指定以及它们与其他类型形参的关系等诸多细节在此不矛赘述，不过你还需要知道其中一点。

Normally if a function is called with no argument for a particular
keyword parameter, the parameter will have the value **NIL**. However,
sometimes you'll want to be able to distinguish between a **NIL** that was
explicitly passed as the argument to a keyword parameter and the
default value **NIL**. To allow this, when you specify a keyword parameter
you can replace the simple name with a list consisting of the name of
the parameter, a default value, and another parameter name, called a
`supplied-p` parameter. The `supplied-p` parameter will be set to true or
false depending on whether an argument was actually passed for that
keyword parameter in a particular call to the function. Here's a
version of foo that uses this feature:

正常情况下，如果所调用的一个函数没有为特定关键字形参传递实参，该形参的值将为
**NIL**。但，有时你可能想要区分作为实参显式传递给关键字形参的 **NIL**
和作为默认值的 **NIL**。为此，在指定一个关键字形参时，可以将那个简单的名称替换成一个包括形参名、默认值和另一个称为 `supplied-p`
形参的列表。这个 `supplied-p`
形参可被设置成真或假，具体取决于实参在特定的函数调用里是否真的被传入相应的关键字形参中。下面是一个使用了该特性的 `foo` 版本：

```lisp
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
```

Now the same calls from earlier yield these results:

之前给出同样的调用现在会产生下面的结果：

```lisp
(foo :a 1 :b 2 :c 3)  ==> (1 2 3 T)
(foo :c 3 :b 2 :a 1)  ==> (1 2 3 T)
(foo :a 1 :c 3)       ==> (1 20 3 T)
(foo)                 ==> (NIL 20 30 NIL)
```

The general selector-function generator, which you can call `where` for
reasons that will soon become apparent if you're familiar with SQL
databases, is a function that takes four keyword parameters
corresponding to the fields in our CD records and generates a selector
function that selects any CDs that match all the values given to
where. For instance, it will let you say things like this:

通用的选择器函数生成器 `where` 是一个函数——如果你熟悉 SQL
数据库的话，就会逐渐明白为什么叫它 `where`
了。它接受对应于我们的 CD
记录字段的四个关键字形参，然后生成一个选择器函数，后者可以选出任何匹配
`where` 子句的 CD。例如，它可以让你写出这样的语句来：

```lisp
(select (where :artist "Dixie Chicks"))
```

or this:

或是这样：

```lisp
(select (where :rating 10 :ripped nil))
```

The function looks like this:

该函数看起来是这样的：

```lisp
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
```

This function returns an anonymous function that returns the logical
**AND** of one clause per field in our CD records. Each clause checks if
the appropriate argument was passed in and then either compares it to
the value in the corresponding field in the CD record or returns `t`,
Lisp's version of truth, if the parameter wasn't passed in. Thus, the
selector function will return `t` only for CDs that match all the
arguments passed to `where`. Note that you need to use a three-item
list to specify the keyword parameter `ripped` because you need to know
whether the caller actually passed `:ripped` nil, meaning, "Select CDs
whose `ripped` field is `nil`," or whether they left out `:ripped`
altogether, meaning "I don't care what the value of the `ripped` field
is."

这个函数返回一个匿名函数，后者返回一个逻辑
**AND**，而其中每个子句分别来自我们 CD
记录中的一个字段。每个子句会检查相应的参数是否被传递进来，然后要么将其跟
CD 记录中对应字段的值相比较，要么在参数没有传进来时返回 `t`，也就是
Lisp 版本的逻辑真。这样，选择器函数将只在 CD 记录匹配所有传递给 `where`
的参数时才返回 `t`。注意到需要使用三元素列表来指定关键字形参
`ripped`，因为你需要知道调用者是否实际传递了 `:ripped nil`，意思是“选择那些
`ripped` 字段为 `nil` 的 CD”，或者是否它们将 `:ripped`
整个扔下不管了，意思是 “我不在乎那个 `ripped` 字段的值”。
