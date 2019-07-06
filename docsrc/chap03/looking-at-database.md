# Looking at the Database Contents（查看数据库的内容）

You can also see the current value of `*db*` whenever you want by typing
`*db*` at the REPL.

无论何时，在 REPL 里输入 `*db*` 都可以看到 `*db*` 的当前值。

```
CL-USER> *db*
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
```
 
However, that's not a very satisfying way of looking at the
output. You can write a `dump-db` function that dumps out the database
in a more human-readable format, like this:

但这种查看输出的方式并不令人满意，可以用一个 `dump-db`
函数来将数据库转储成一个像下面这样的更适合人类阅读习惯的格式。


```
TITLE:    Home
ARTIST:   Dixie Chicks
RATING:   9
RIPPED:   T

TITLE:    Fly
ARTIST:   Dixie Chicks
RATING:   8
RIPPED:   T

TITLE:    Roses
ARTIST:   Kathy Mattea
RATING:   7
RIPPED:   T
```

The function looks like this:

该函数如下所示：

```lisp
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))
```

This function works by looping over all the elements of `*db*` with the
**DOLIST** macro, binding each element to the variable cd in turn. For
each value of `cd`, you use the **FORMAT** function to print it.

该函数的工作原理是使用 **DOLIST** 宏在 `*db*`
的所有元素上循环，依次绑定每个元素到变量
`cd` 上。而后使用 **FORMAT** 函数打印出每个 `cd` 的值。

Admittedly, the **FORMAT** call is a little cryptic. However, **FORMAT** isn't
particularly more complicated than C or Perl's `printf` function or
Python's `string-%` operator. In Chapter 18 I'll discuss **FORMAT** in
greater detail. For now we can take this call bit by bit. As you saw
in Chapter 2, **FORMAT** takes at least two arguments, the first being the
stream where it sends its output; `t` is shorthand for the stream
`*standard-output*`.

无可否认，这个 **FORMAT** 调用多少显得有些晦涩。尽管如此，但 **FORMAT**
却并不比 C 或 Perl 的 `printf` 函数或者 Python 的 `string-%`
操作符更复杂。第 18 章将进一步讨论 **FORMAT**
的细节，目前我们只需记住这个调用就可以了。第 2
章所述，**FORMAT** 至少接受两个实参，第一个是它用来发送输出的流，`t`
是标准输出流（`*standard-output*`）的简称。

The second argument to **FORMAT** is a format string that can contain both
literal text and directives telling **FORMAT** things such as how to
interpolate the rest of its arguments. Format directives start with `~`
(much the way printf's directives start with `%`). **FORMAT** understands
dozens of directives, each with their own set of options. However,
for now I'll just focus on the ones you need to write `dump-db`.

**FORMAT** 的第二个参数是一个格式字符串，内容既包括字面文本，也包括那些告诉
**FORMAT** 如何插入其余参数等信息的指令。格式指令以 `~`
开始（就像是 `printf` 指令以 `%` 开始那样）。**FORMAT**
能够接受大量的指令，每一个都有自己的选项集。 但目前我将只关注那些对编写
`dump-db` 有用的选项。

The `~a` directive is the aesthetic directive; it means to consume one
argument and output it in a human-readable form. This will render
keywords without the leading `:` and strings without quotation
marks. For instance:

`~a` 指令是美化指令,它的意图是消耗一个实参，然后将其输出成人类可读的形式。这将使关键字被渲染成不带前导冒号的形式，而字符串也不再有引号了。例如：


```lisp
CL-USER> (format t "~a" "Dixie Chicks")
Dixie Chicks
NIL
```

or:

或是：

```lisp
CL-USER> (format t "~a" :title)
TITLE
NIL
```

The `~t` directive is for tabulating. The `~10t` tells **FORMAT** to
emit enough spaces to move to the tenth column before processing the
next `~a`. A `~t` doesn't consume any arguments.

`~t` 指令用于制表。`~10t` 告诉 **FORMAT** 产生足够的空格，以确保在处理下一个
`~a` 之前将光标移动 10 列。`~t` 指令不消耗任何参数。

```lisp
CL-USER> (format t "~a:~10t~a" :artist "Dixie Chicks")
ARTIST:   Dixie Chicks
NIL
```

Now things get slightly more complicated. When **FORMAT** sees `~{` the next
argument to be consumed must be a list. **FORMAT** loops over that list,
processing the directives between the `~{` and `~}`, consuming as many
elements of the list as needed each time through the list. In `dump-db`,
the **FORMAT** loop will consume one keyword and one value from the list
each time through the loop. The `~%` directive doesn't consume any
arguments but tells **FORMAT** to emit a newline. Then after the `~}` ends
the loop, the last `~%` tells **FORMAT** to emit one more newline to put a
blank line between each CD.

现在事情变得稍微复杂一些了。当 **FORMAT** 看到 `~{`
的时候，下一个被消耗的实参必须是一个列表。**FORMAT** 在列表上循环操作，处理位于
`~{` 和 `~}` 之间的指令，同时在每次需要时，从列表上消耗掉尽可能多的元素。在 `dump-db`
里，**FORMAT** 循环将在每次循环时从列表上消耗一个关键字和一个值。`~%`
指令并不消耗任何实参，而只是告诉 **FORMAT**
来产生一个换行。然后在 `~}` 循环结束以后，最后一个 `~%` 告诉 **FORMAT**
再输出一个额外的换行，以便在每个 CD 的数据之间产生一个空行。

Technically, you could have also used **FORMAT** to loop over the database
itself, turning our `dump-db` function into a one-liner.

从技术上来讲，也可以使用 **FORMAT** 在整个数据库本身上循环，从而将我们的
`dump-db` 函数变成只有一行。


```lisp
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
```
  
That's either very cool or very scary depending on your point of view.

这件事究竟是酷还是恐怖，完全看你怎么想。
