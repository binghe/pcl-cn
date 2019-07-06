# Filling CDs（录入 CD）

A single record, however, does not a database make. You need some
larger construct to hold the records. Again, for simplicity's sake, a
list seems like a good choice. Also for simplicity you can use a
global variable, `*db*`, which you can define with the **DEFVAR**
macro. The asterisks (`*`) in the name are a Lisp naming convention for
global variables.

只有单一记录还不能算是一个数据库，需要一些更大的结构来保存记录。出于简化目的，使用列表似乎也还不错。同样出于简化目的，也可以使用一个全局变量
`*db*`，它可以用 **DEFVAR** 宏来定义。名字中的星号是 Lisp 的全局变量命名约定。


```lisp
(defvar *db* nil)
```

You can use the **PUSH** macro to add items to *db*. But it's probably
a good idea to abstract things a tiny bit, so you should define a
function add-record that adds a record to the database.

可以使用 **PUSH** 宏为 `*db*` 添加新的项。但稍微做得抽象一些可能更好，因此可以定义一个函数
`add-record` 来给数据库增加一条记录。

```lisp
(defun add-record (cd) (push cd *db*))
```

Now you can use `add-record` and `make-cd` together to add CDs to the database.

现在可以将 `add-record` 和 `make-cd` 一起使用，来为数据库添加新的 CD 记录了。

```lisp
CL-USER> (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
((:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
CL-USER> (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
CL-USER> (add-record (make-cd "Home" "Dixie Chicks" 9 t))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
```

The stuff printed by the REPL after each call to `add-record` is the
return value, which is the value returned by the last expression in
the function body, the **PUSH**. And **PUSH** returns the new value of the
variable it's modifying. So what you're actually seeing is the value
of the database after the record has been added.

那些每次调用 `add-record` 以后 REPL
所打印出来的东西是返回值，也就是函数体中最后一个表达式 **PUSH**
所返回的值，并且 **PUSH**
返回它正在修改的变量的新值。因此你看到的其实是每次新记录被添加以后整个数据库的值。
