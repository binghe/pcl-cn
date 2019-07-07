# Updating Existing Records--Another Use for WHERE（更新已有的记录——WHERE 再战江湖）

Now that you've got nice generalized select and where functions,
you're in a good position to write the next feature that every
database needs--a way to update particular records. In SQL the update
command is used to update a set of records matching a particular where
clause. That seems like a good model, especially since you've already
got a where-clause generator. In fact, the update function is mostly
just the application of a few ideas you've already seen: using a
passed-in selector function to choose the records to update and using
keyword arguments to specify the values to change. The main new bit is
the use of a function **MAPCAR** that maps over a list, `*db*` in this case,
and returns a new list containing the results of calling a function on
each item in the original list.

有了完美通用的 `select` 和 `where`
函数，是时候开始编写下一个所有数据库都需要的特性——更新特定记录的方法了。在
SQL 中，`update` 命令被用于更新一组匹配特定 `where`
子句的记录。这听起来像是个很好的模型，尤其是当已经有了一个 `where`
子句生成器时。事实上，`update`
函数只是你已经见过的一些思路的再次应用：使用一个通过参数传递的选择器函数来选取需要更新的记录，再使用关键字形参来指定需要改变的值。这里主要出现的新内容是对
**MAPCAR** 函数的使用，其映射在一个列表上（这里是 `*db*`），然后返回一个新的列表，其中含有在原来列表的每个
元素上调用一个函数所得到的结果。

```lisp
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))
```

One other new bit here is the use of **SETF** on a complex form such as
`(getf row :title)`. I'll discuss **SETF** in greater detail in Chapter 6,
but for now you just need to know that it's a general assignment
operator that can be used to assign lots of "places" other than just
variables. (It's a coincidence that **SETF** and **GETF** have such similar
names--they don't have any special relationship.) For now it's enough
to know that after `(setf (getf row :title) title)`, the plist
referenced by row will have the value of the variable title following
the property name `:title`. With this update function if you decide that
you really dig the Dixie Chicks and that all their albums should go to
11, you can evaluate the following form:

这里的另一个新内容是 **SETF** 用在了诸如 `(getf row :title)`
这样的复杂形式上。第 6 章将细致地讨论
**SETF**，目前只需知道它是一个通用的赋值操作符，可被用于对各种 “位置”
而不只是对变量进行赋值即可。（**SETF** 和 **GETF**
具有相似的名字，但这纯属巧合，两者之间并没有特别的关系。）眼下知道执行
`(setf (getf row :title) title)` 以后的结果就可以了：由 `row`
所引用的 plist 将具有紧跟着属性名 `:title` 后面的那项变量 `title`
的值。有了这个 `update` 函数，如果你觉得自己真的很喜欢 Dixie
Chicks，并且他们的所有专辑的评级应该升到 11，那么可以对下列形式求值：

```lisp
CL-USER> (update (where :artist "Dixie Chicks") :rating 11)
NIL
```

And it is so.

这样就可以了。

```lisp
CL-USER> (select (where :artist "Dixie Chicks"))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 11 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 11 :RIPPED T))
```

You can even more easily add a function to delete rows from the database.

甚至可以更容易地添加一个函数来从数据库里删除行。

```lisp
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
```
  
The function **REMOVE-IF** is the complement of **REMOVE-IF-NOT**; it returns
a list with all the elements that do match the predicate
removed. Like **REMOVE-IF-NOT**, it doesn't actually affect the list
it's passed but by saving the result back into `*db*`, `delete-rows`
actually changes the contents of the database.

函数 **REMOVE-IF** 的功能跟 **REMOVE-IF-NOT**
正好相反，在它所返回的列表中，所有确实匹配谓词的元素都被删掉的。和
**REMOVE-IF-NOT** 一样，它不会实际地影响传入的那个列表，但是通过将结果重新保存到
`*db*` 中，`delete-rows` 事实上改变了数据库的内容。
