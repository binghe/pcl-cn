# Generic Functions and Methods（广义函数和方法）

A generic function defines an abstract operation, specifying its name
and a parameter list but no implementation. Here, for example, is how
you might define a generic function, draw, that will be used to draw
different kinds of shapes on the screen:

广义函数定义了抽象操作，指定了其名字和一个参数列表，但不提供实现。例如，下面就是你可能定义广义函数
`draw` 的方式，它将用来在屏幕上绘制不同的形状：

```lisp
(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen."))
```

I'll discuss the syntax of **DEFGENERIC** in the next section; for now
just note that this definition doesn't contain any actual code.

我将在下一节里讨论 **DEFGENERIC**
的语法，目前只需注意该定义并不含有任何实际代码。

A generic function is generic in the sense that it can--at least in
theory--accept any objects as arguments. However, by itself a generic
function can't actually do anything; if you just define a generic
function, no matter what arguments you call it with, it will signal an
error. The actual implementation of a generic function is provided by
methods. Each method provides an implementation of the generic
function for particular classes of arguments. Perhaps the biggest
difference between a generic function-based system and a
message-passing system is that methods don't belong to classes; they
belong to the generic function, which is responsible for determining
what method or methods to run in response to a particular invocation.

广义函数的广义性至少在理论上体现在，它可以接受任何对象作为参数。 不过，广义函数本身并不能做任何事。如果你只是定义广义函数，那么无论用什么参数来调用它，它都将会报错。广义函数的实际实现是由方法（method）提供的。每一个方法提供了广义函数用于特定参数类的实现。也许在一个基于广义函数的系统和一个消息传递系统之间最大的区别在于方法并不属于类，它们属于广义函数，其负责在一个特定调用中检测哪个或哪些方法将被运行。

Methods indicate what kinds of arguments they can handle by
specializing the required parameters defined by the generic
function. For instance, on the generic function `draw`, you might define
one method that specializes the `shape` parameter for objects that are
instances of the class `circle` while another method specializes `shape`
for objects that are instances of the class `triangle`. They would look
like this, eliding the actual drawing code:

方法通过特化那些由广义函数所定义的必要参数，来表达它们可以处理的参数类型。例如，在广义函数
`draw` 中，你可以定义一个方法来特化 `shape`
参数，使其用于 `circle` 类的实例对象；而另一个方法则将 `shape`
特化成 `triangle` 类的实例对象。去掉实际的绘图代码以后，它们如下所示：

```lisp
(defmethod draw ((shape circle))
  ...)

(defmethod draw ((shape triangle))
  ...)
```

When a generic function is invoked, it compares the actual arguments
it was passed with the specializers of each of its methods to find the
applicable methods--those methods whose specializers are compatible
with the actual arguments. If you invoke draw, passing an instance of
circle, the method that specialized shape on the class circle is
applicable, while if you pass it a triangle, then the method that
specializes shape on the class triangle applies. In simple cases, only
one method will be applicable, and it will handle the invocation. In
more complex cases, there may be multiple methods that apply; they're
then combined, as I'll discuss in the section "Method Combination,"
into a single effective method that handles the invocation.

当一个广义函数被调用时，它将那些被传递的实际参数与它的每个方法的特化符
进行比较，找出可应用（applicable）的方法，即那些特化符与实际参数相兼容
的方法。如果你调用
`draw` 并传递一个 `circle` 的实例，那么在 `circle`
类上特化了 `shape` 的方法将是可应用的；而如果你传递了一个 `triangle`
实例，那么在 `triangle` 上特化了 `shape`
的方法将被应用。在简单的情况下，只有一个方法是可应用的，并且它将处理该调用。在复杂的情况下，可能有多个方法均可应用，它们随后将被组合起来──我将在
16.5 节里进行讨论，成为一个有效的（effective）方法来处理该调用。

You can specialize a parameter in two ways--usually you'll specify a
class that the argument must be an instance of. Because instances of a
class are also considered instances of that class's superclasses, a
method with a parameter specialized on a particular class can be
applicable whenever the corresponding argument is a direct instance of
the specializing class or of any of its subclasses. The other kind of
specializer is a so-called **EQL** specializer, which specifies a
particular object to which the method applies.

你可以用两种方式来特化参数。通常你将指定一个类，其参数必须是该类的实例。由于一个类的实例也被视为该类的所有基类的实例，因此一个带有特化了某个特定类的参数的方法可以被应用在对应参数无论是该特定类的直接实例或是该类的任何子类的实例上。另一种类型的特化符是所谓的
**EQL** 特化符，其指定了方法所应用的特定对象。

When a generic function has only methods specialized on a single
parameter and all the specializers are class specializers, the result
of invoking a generic function is quite similar to the result of
invoking a method in a message-passing system--the combination of the
name of the operation and the class of the object on which it's
invoked determines what method to run.

当一个广义函数只具有特化在单一参数上的方法并且所有特化符都是类特化符时，调用广义参数的结果跟在一个消息传递系统下调用方法的结果非常相似──操作的名字与调用时对象的类的组合决定了哪个方法被运行。

However, reversing the order of lookup opens up possibilities not
found in message-passing systems. Generic functions support methods
that specialize on multiple parameters, provide a framework that makes
multiple inheritance much more manageable, and let you use declarative
constructs to control how methods are combined into an effective
method, supporting several common usage patterns without a lot of
boilerplate code. I'll discuss those topics in a moment. But first you
need to look at the basics of the two macros used to define the
generic functions **DEFGENERIC** and **DEFMETHOD**.

尽管如此，相反的方法查找顺序带来了消息传递系统所没有的可能性。广义函数支持特化在多个参数上的方法，提供了一个使多继承更具有可管理性的框架，并且允许你使用声明性的构造来控制方法如何组合成有效方法，从而在无需使用大量模板代码的情况下直接支持几种常用的设计模式。我将很快讨论到这些主题。但首先你需要了解两个用来定义广义函数的宏
**DEFGENERIC** 和 **DEFMETHOD** 的一些基础。

