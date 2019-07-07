# Improving the User Interaction（改进用户交互）

While our `add-record` function works fine for adding records, it's a
bit Lispy for the casual user. And if they want to add a bunch of
records, it's not very convenient. So you may want to write a function
to prompt the user for information about a set of CDs. Right away you
know you'll need some way to prompt the user for a piece of
information and read it. So let's write that.

尽管我们的 `add-record`
函数在添加记录方面做得很好，但对于普通用户来说却仍显得过于 Lisp
化了。并且如果他们想要添加大量的记录，这种操作也并不是很方便。因此你可能想要写一个函数来提示用户输入一组
CD 信息。这就意味着需要以某种方式来提示用户输入一条信息，然后读取它。下面让我们来写这个。

```lisp
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
```

You use your old friend **FORMAT** to emit a prompt. Note that there's no
`~%` in the format string, so the cursor will stay on the same line. The
call to **FORCE-OUTPUT** is necessary in some implementations to ensure
that Lisp doesn't wait for a newline before it prints the prompt.

你用老朋友 **FORMAT**
来产生一个提示。注意到格式字符串里并没有 `~%`，因此光标将停留在同一行里。对
**FORCE-OUTPUT** 的调用在某些实现里是必需的，这是为了确保
Lisp 在打印提示信息之前不会等待换行。

Then you can read a single line of text with the aptly named **READ-LINE**
function. The variable `*query-io*` is a global variable (which you can
tell because of the `*` naming convention for global variables) that
contains the input stream connected to the terminal. The return value
of prompt-read will be the value of the last form, the call to
**READ-LINE**, which returns the string it read (without the trailing
newline.)

然后就可以使用名副其实的 **READ-LINE** 函数来读取单行文本了。变量 `*query-io*`
是一个含有关联到当前终端的输入流的全局变量（通过星号命名约定你也可以看出这点来）。`prompt-read`
的返回值将是其最后一个形式，即调用 **READ-LINE**
所得到的值，也就是它所读取的字符串（不包括结尾的换行）。

You can combine your existing `make-cd` function with `prompt-read` to
build a function that makes a new CD record from data it gets by
prompting for each value in turn.

你可以将已有的 `make-cd` 函数跟 `prompt-read`
组合起来，从而构造出一个函数，可以从依次提示输入每个值得到的数据中建立新的
CD 记录。

```lisp
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))
```

That's almost right. Except prompt-read returns a string, which, while
fine for the Title and Artist fields, isn't so great for the Rating
and Ripped fields, which should be a number and a boolean. Depending
on how sophisticated a user interface you want, you can go to
arbitrary lengths to validate the data the user enters. For now
let's lean toward the quick and dirty: you can wrap the prompt-read
for the rating in a call to Lisp's **PARSE-INTEGER** function, like this:
  
这样已经差不多正确了。只是 `prompt-read` 总是返回字符串，对于 Title
和 Artist 字段来说可以，但对于 Rating 和 Ripped
字段来说就不太好了，它们应该是数字和布尔值。花在验证用户输入数据上的努力可以是无止境的而这取决于所要实现的用户接口专业程度。目前我们倾向于一种快餐式办法，你可以将关于评级的那个
`prompt-read` 包装在一个 Lisp 的 **PARSE-INEGER** 函数里，就像这样：

```lisp
(parse-integer (prompt-read "Rating"))
```

Unfortunately, the default behavior of **PARSE-INTEGER** is to signal an
error if it can't parse an integer out of the string or if there's any
non-numeric junk in the string. However, it takes an optional keyword
argument `:junk-allowed`, which tells it to relax a bit.

不幸的是，**PARSE-INTEGER**
的默认行为是当它无法从字符串中正确解析出整数，或者字符串里含有任何非数字的垃圾时直接报错。不过，它接受一个可选的关键字参数
`:junk-allowed`，可以让其适当地宽容一些。

```lisp
(parse-integer (prompt-read "Rating") :junk-allowed t)
```

But there's still one problem: if it can't find an integer amidst all
the junk, **PARSE-INTEGER** will return **NIL** rather than a number. In
keeping with the quick-and-dirty approach, you may just want to call
that 0 and continue. Lisp's **OR** macro is just the thing you need
here. It's similar to the "short-circuiting" `||` in Perl, Python, Java,
and C; it takes a series of expressions, evaluates them one at a time,
and returns the first non-nil value (or **NIL** if they're all **NIL**). So
you can use the following:

但还有一个问题：如果无法在所有垃圾里找出整数的话，**PARSE-INTEGER** 将返回
**NIL** 而不是整数。为了保持这个快餐式的思路，你可以把这种情况当作 0
来看待。Lisp 的 **OR** 宏就是你在此时所需要的。它与 Perl、Python、Java
以及 C 中的 “短路” 符号 `||`
很类似，它接受一系列表达式，依次对它们求值，然后返回第一个非空的值（或者空值，如果它们全部是空值的话）。所以可以使用下面这样的语句：

```lisp
(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
```
to get a default value of 0.

来得到一个缺省值 0。

Fixing the code to prompt for Ripped is quite a bit simpler. You can
just use the Common Lisp function **Y-OR-N-P**.

修复 Ripped 提示的代码就更容易了，只需使用 Common Lisp 的 **Y-OR-N-P**
函数：

```lisp
(y-or-n-p "Ripped [y/n]: ")
```

In fact, this will be the most robust part of prompt-for-cd, as
**Y-OR-N-P** will reprompt the user if they enter something that doesn't
start with y, Y, n, or N.

事实上，这将是 `prompt-for-cd` 中最健壮的部分，因为 **Y-OR-N-P**
会在你输入了没有以 y、Y、n，或者 N 开始的内容时重新提示输入。

Putting those pieces together you get a reasonably robust
`prompt-for-cd` function.

将所有这些内容放在一起，就得到了一个相当健壮的 `prompt-for-cd` 函数了。

```lisp
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))
```

Finally, you can finish the "add a bunch of CDs" interface by wrapping
`prompt-for-cd` in a function that loops until the user is done. You
can use the simple form of the **LOOP** macro, which repeatedly executes a
body of expressions until it's exited by a call to **RETURN**. For
example:

最后可以将 `prompt-for-cd` 包装在一个不停循环直到用户完成的函数里，以此来搞
定这个 “添加大量 CD” 的接口。可以使用 **LOOP**
宏的一种简单形式，它不断执行一个表达式体，最后通过调用 **RETURN** 来退出。例如：

```lisp
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))
```
      
Now you can use `add-cds` to add some more CDs to the database.

现在可以使用 `add-cds` 来添加更多 CD 到数据库里了。

```
CL-USER> (add-cds)
Title: Rockin' the Suburbs
Artist: Ben Folds
Rating: 6
Ripped  [y/n]: y
Another?  [y/n]: y
Title: Give Us a Break
Artist: Limpopo
Rating: 10
Ripped  [y/n]: y
Another?  [y/n]: y
Title: Lyle Lovett
Artist: Lyle Lovett
Rating: 9
Ripped  [y/n]: y
Another?  [y/n]: n
NIL
```
