# Walking a Directory Tree（遍历一个目录树）

Finally, to round out this library, you can implement a function
called `walk-directory`. Unlike the functions defined previously, this
function doesn't need to do much of anything to smooth over
implementation differences; it just needs to use the functions you've
already defined. However, it's quite handy, and you'll use it several
times in subsequent chapters. It will take the name of a directory and
a function and call the function on the pathnames of all the files
under the directory, recursively. It will also take two keyword
arguments: `:directories` and `:test`. When `:directories` is true, it will
call the function on the pathnames of directories as well as regular
files. The `:test` argument, if provided, specifies another function
that's invoked on each pathname before the main function is; the main
function will be called only if the test function returns true.

最后，为了完成这个库，你可以实现一个称为 `walk-directory`
的函数。与前面定义的那些函数不同，这个函数不需要做任何事情来消除实现间的区别，它只需要用到你已经定义的那些函数。尽管如此，该函数很有用，你将在后续几章里多次用到它。它接受一个目录的名字和一个函数，并在该目录下所有文件的路径名上递归地调用该函数。它还接受两个关键字参数：
`:directories`
和 `:test`。当 `:directories`
为真时，它将在所有目录的路径名和正规文件上调用该函数。如果有 `:test`
参数，它指定另一个函数，在调用主函数之前在每一个路径名上调用该函数，主
函数只有当测试参数返回真时才会被调用。`:test`
参数的默认值是一个总是返回真的函数，它是通过调用标准函数
**CONSTANTLY** 而生成的。

```lisp
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
```

Now you have a useful library of functions for dealing with
pathnames. As I mentioned, these functions will come in handy in later
chapters, particularly Chapters 23 and 27, where you'll use
walk-directory to crawl through directory trees containing spam
messages and MP3 files. But before we get to that, though, I need to
talk about object orientation, the topic of the next two chapters.

现在你有了一个用于处理路径名的有用的函数库。正如我提到的那样，这些函数在后面的章节里将会很有用，尤其是第
23 章和第 27 章，在那里你将使用 `walk-directory` 在混合了垃圾信息和
MP3 文件的目录树中艰难前行，但在我们到达那里之前，我还需要在接下来的两章中谈论一下面向对象。
