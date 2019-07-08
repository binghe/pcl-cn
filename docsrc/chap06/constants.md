# Constants（常量）

One other kind of variable I haven't mentioned at all is the
oxymoronic "constant variable." All constants are global and are
defined with **DEFCONSTANT**. The basic form of **DEFCONSTANT**
is like **DEFPARAMETER**.

我尚未提到的另一种类型的变量是所谓的
“常值变量”。所有的常量都是全局的，并且使用 **DEFCONSTANT**
的定义，**DEFCONSTANT** 的基本形式与 **DEFPARAMETER** 相似。

```lisp
(defconstant name initial-value-form [ documentation-string ])
```

As with **DEFVAR** and **DEFPARAMETER**, **DEFCONSTANT** has a global effect on
the name used--thereafter the name can be used only to refer to the
constant; it can't be used as a function parameter or rebound with any
other binding form. Thus, many Lisp programmers follow a naming
convention of using names starting and ending with `+` for
constants. This convention is somewhat less universally followed than
the `*`-naming convention for globally special names but is a good idea
for the same reason.

与 **DEFVAR** 和 **DEFPARAMETER** 相似，**DEFCONSTANT**
在其使用的名字上产生了一个全局效果——从此该名字仅被用于指向常量；它不能被用作函数形参或是用任何其他的绑定形式进行重绑定。因此，许多
Lisp 程序员遵循了一个命名约定，用以 `+`
开始或结尾的名字来表示常量，这一约定在某种程度上不像全局特殊名字的 `*`
命名约定那样流行，但同样也不错。

Another thing to note about **DEFCONSTANT** is that while the language
allows you to redefine a constant by reevaluating a **DEFCONSTANT** with a
different `initial-value-form`, what exactly happens after the
redefinition isn't defined. In practice, most implementations will
require you to reevaluate any code that refers to the constant in
order to see the new value since the old value may well have been
inlined. Consequently, it's a good idea to use **DEFCONSTANT** only to
define things that are really constant, such as the value of NIL. For
things you might ever want to change, you should use **DEFPARAMETER**
instead.

关于 **DEFCONSTANT**，需要注意的另一点是，尽管语言允许通过重新求值一个带有一个初始值形式的
**DEFCONSTANT**
来重定义一个常量，但在重定义之后究竟发生什么是没有定义的。在实践上，多数实现将要求任何对引用了该常量的代码进行求值以便它们能看到新值，因为老的值可能已经被内联到代码中了。因此最好只用
**DEFCONSTANT**
来定义那些真正是常量的东西，例如π的值。而对于那些可能想改变的东西，则应转而使用
**DEFPARAMETER**。
