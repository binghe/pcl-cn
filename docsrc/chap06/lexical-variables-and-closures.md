# Lexical Variables and Closures（词法变量和闭包）

By default all binding forms in Common Lisp introduce lexically scoped
variables. Lexically scoped variables can be referred to only by code
that's textually within the binding form. Lexical scoping should be
familiar to anyone who has programmed in Java, C, Perl, or Python
since they all provide lexically scoped "local" variables. For that
matter, Algol programmers should also feel right at home, as Algol
first introduced lexical scoping in the 1960s.

默认情况下，Common Lisp
中所有的绑定形式都将引入词法作用域变量。词法作用域的变量只能由那些在文本上位于绑定形式之内的代码所引用。词法作用域应该被那些曾经使用
Java、C、Perl 或者 Python
来编程的人们所熟悉，因为它们都提供词法作用域的局部变量。如此说来，Algol
程序员们也该对其感到自然才是，因为 Algol 在 20 世纪 60 年代首先引入了词法作用域。

However, Common Lisp's lexical variables are lexical variables with a
twist, at least compared to the original Algol model. The twist is
provided by the combination of lexical scoping with nested
functions. By the rules of lexical scoping, only code textually within
the binding form can refer to a lexical variable. But what happens
when an anonymous function contains a reference to a lexical variable
from an enclosing scope? For instance, in this expression:

尽管如此，但 Common Lisp
的词法变量还是带有一些变化的，至少和最初的 Algol
模型相比是这样。变化之处在于将词法作用域和嵌套函数一起使用时，按照词法作用域的规则，只有文本上位于绑定形式之内的代码可以指向一个词法变量。但是当一个匿名函数含有一个对来自封闭作用域之内词法变量的引用时，将会发生什么呢？例如，在下面的表达式中：

```lisp
(let ((count 0)) #'(lambda () (setf count (1+ count))))
```

the reference to count inside the LAMBDA form should be legal
according to the rules of lexical scoping. Yet the anonymous function
containing the reference will be returned as the value of the LET form
and can be invoked, via FUNCALL, by code that's not in the scope of
the LET. So what happens? As it turns out, when count is a lexical
variable, it just works. The binding of count created when the flow of
control entered the LET form will stick around for as long as needed,
in this case for as long as someone holds onto a reference to the
function object returned by the LET form. The anonymous function is
called a closure because it "closes over" the binding created by the
LET.

根据词法作用域规则，**LAMBDA** 形式中对 `count`
的引用应该是合法的，而这个含有引用的匿名函数将被作为 **LET**
形式的值返回，并可能会通过 **FUNCALL** 被不在 **LET**
作用域之内的代码所调用。这样会发生什么呢？正如你将看到的那样，当 `count`
是一个词法变量时，情况一切正常。本例中，当控制流进入 **LET** 形式时所创建的 `count`
绑定将被尽可能地保留下来，只要某处保持了一个对 **LET**
形式所返回的函数对象的引用即可。这个匿名函数被称为一个闭包，因为它 “封闭包装”
了由 **LET** 创建的绑定。

The key thing to understand about closures is that it's the binding,
not the value of the variable, that's captured. Thus, a closure can
not only access the value of the variables it closes over but can also
assign new values that will persist between calls to the closure. For
instance, you can capture the closure created by the previous
expression in a global variable like this:

理解闭包的关键在于，被捕捉的是绑定而不是变量的值。因此，一个闭包不仅可以访问它所闭合的变量的值，还可以对其赋予可在闭包被调用时不断变化的新值。例如，可以像下面这样将前面的表达式所创建的闭包捕捉到一个全局变量里：

```lisp
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
```

Then each time you invoke it, the value of count will increase by one.

然后每次当你调用它时，`count` 的值将被加 1：

```lisp
CL-USER> (funcall *fn*)
1
CL-USER> (funcall *fn*)
2
CL-USER> (funcall *fn*)
3
```

A single closure can close over many variable bindings simply by
referring to them. Or multiple closures can capture the same
binding. For instance, the following expression returns a list of
three closures, one that increments the value of the closed over count
binding, one that decrements it, and one that returns the current
value:

单一闭包可以简单地通过引用变量来闭合许多变量绑定，或是多个闭合可以捕捉相同的绑定。例如，下面的表达式返回由三个闭包所组成的列表，一个可以递增其所闭合的
`count` 绑定的值，另一个可以递减它，还有一个返回它的当前值。

```lisp
(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))
```
