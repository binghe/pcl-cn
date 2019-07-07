# Saving and Loading the Database（保存和加载数据库）

Having a convenient way to add records to the database is nice. But
it's not so nice that the user is going to be very happy if they have
to reenter all the records every time they quit and restart
Lisp. Luckily, with the data structures you're using to represent the
data, it's trivially easy to save the data to a file and reload it
later. Here's a `save-db` function that takes a filename as an argument
and saves the current state of the database:

用一种便利的方式来给数据库添加新记录是件好事。但如果让用户不得不在每次退出并重启
Lisp 以后再重新输入所有记录，他们是绝对不会高兴的。幸好，借助用来表示数据的数据结构，可以相当容易地将数据保存在文件里并在稍后重新加载。下面是一个
`save-db` 函数，它接受一个文件名作为参数并且保存当前数据库的状态：

```lisp
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))
```

The **WITH-OPEN-FILE** macro opens a file, binds the stream to a variable,
executes a set of expressions, and then closes the file. It also
makes sure the file is closed even if something goes wrong while
evaluating the body. The list directly after **WITH-OPEN-FILE** isn't a
function call but rather part of the syntax defined by
**WITH-OPEN-FILE**. It contains the name of the variable that will hold
the file stream to which you'll write within the body of
**WITH-OPEN-FILE**, a value that must be a file name, and then some
options that control how the file is opened. Here you specify that
you're opening the file for writing with `:direction :output` and that
you want to overwrite an existing file of the same name if it exists
with `:if-exists :supersede`.

**WITH-OPEN-FILE** 宏会打开一个文件，将文件流绑定到一个变量上，执行一组表达式，然后再关闭这个文件。它还可以保证即便在表达式体求值出错时也可以正确关闭文件。紧跟着
**WITH-OPEN-FILE** 的列表并非函数调用而是 **WITH-OPEN-FILE**
语法的一部分。它含有用来保存要在 **WITH-OPEN-FILE**
主体中写入的文件流的变量名，这个值必须是文件名，紧随其后是一些控制如何打开文件的选项。这里用
`:direction :output` 指定了正在打开一个用于写入的文件，以及用
`:if-exists :supersede` 说明当存在同名的文件时想要覆盖已存在的文件。

Once you have the file open, all you have to do is print the contents
of the database with `(print *db* out)`. Unlike **FORMAT**, **PRINT** prints
Lisp objects in a form that can be read back in by the Lisp
reader. The macro **WITH-STANDARD-IO-SYNTAX** ensures that certain
variables that affect the behavior of **PRINT** are set to their standard
values. You'll use the same macro when you read the data back in to
make sure the Lisp reader and printer are operating compatibly.

一旦已经打开了文件，所需做的就只是使用 `(print *db* out)`
将数据库的内容打印出来。跟 **FORMAT** 不同的是，**PRINT** 会将 Lisp
对象打印成一种可以被 Lisp 读取器读回来的形式。宏 **WITH-STANDARD-IO-SYNTAX**
可以确保那些影响 **PRINT** 行为的特定变量可以被设置成它们的标准值。当把数据读回来时，你将使用同样的宏来确保
Lisp 读取器和打印器的操作彼此兼容。

The argument to `save-db` should be a string containing the name of the
file where the user wants to save the database. The exact form of the
string will depend on what operating system they're using. For
instance, on a Unix box they should be able to call `save-db` like this:

`save-db` 的实参应该是一个含有用户打算用来保存数据库的文件名的字符串。该字符串的确切形式取决于正在使用什么操作系统。例如，在
Unix 系统上可能会这样调用 `save-db`：

```lisp
CL-USER> (save-db "~/my-cds.db")
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED T)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED
  T)
 (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 9 :RIPPED T))
```

On Windows, the filename might be something like "c:/my-cds.db" or
"c:\\my-cds.db."

在 Windows 下，文件名可能会是 “c:/my-cds.db” 或 “c:\\my-cds.db”。

You can open this file in any text editor to see what it looks
like. You should see something a lot like what the REPL prints if you
type `*db*`.

你可以在任何文本编辑器里打开这个文件来查看它的内容。所看到的东西应该和直接在
REPL 里输入 `*db*` 时看到的东西差不多。

The function to load the database back in is similar.

将数据加载回数据库的函数也是差不多的样子：

```lisp
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
    (setf *db* (read in)))))
```

This time you don't need to specify `:direction` in the options to
**WITH-OPEN-FILE**, since you want the default of `:input`. And instead of
printing, you use the function **READ** to read from the stream in. This
is the same reader used by the REPL and can read any Lisp expression
you could type at the REPL prompt. However, in this case, you're
just reading and saving the expression, not evaluating it. Again,
the **WITH-STANDARD-IO-SYNTAX** macro ensures that **READ** is using the
same basic syntax that save-db did when it **PRINT**ed the data.

这次不需要在 **WITH-OPEN-FILE** 的选项里指定 `:direction`
了，因为你要的是默认值 `:input`。并且与打印相反，这次要做的是使用函数 **READ**
来从流中读入。这是与 REPL 使用的相同的读取器，可以读取你在 REPL 提示符下输入的
Lisp 表达式。但本例中只是读取和保存表达式，并不会对它求值。**WITH-STANDARD-IO-SYNTAX**
宏再一次确保 **READ** 使用和 `save-db` 在打印数据时相同的基本语法。

The **SETF** macro is Common Lisp's main assignment operator. It sets its
first argument to the result of evaluating its second argument. So in
`load-db` the `*db*` variable will contain the object read from the file,
namely, the list of lists written by `save-db`. You do need to be
careful about one thing--`load-db` clobbers whatever was in `*db*` before
the call. So if you've added records with `add-record` or `add-cds` that
haven't been saved with save-db, you'll lose them.

**SETF** 宏是 Common Lisp
最主要的赋值操作符。它将其第一个参数设置成其第二个参数的求值结果。因此在
`load-db` 里，变量 `*db*` 将含有从文件中读取的对象，也就是由
`save-db` 所写入的那些列表组成的列表。需要特别注意一件事——`load-db`
会破坏其被调用之前 `*db*` 里面的东西。因此，如果已经用 `add-record`
或者 `add-cds` 添加了尚未用 `save-db` 保存的记录，你将失去它们。
