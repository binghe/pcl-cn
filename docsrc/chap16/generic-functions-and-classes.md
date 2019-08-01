# Generic Functions and Classes（广义函数和类）

The fundamental idea of object orientation is that a powerful way to
organize a program is to define data types and then associate
operations with those data types. In particular, you want to be able
to invoke an operation and have the exact behavior determined by the
type of the object or objects on which the operation was invoked. The
classic example used, seemingly by all introductions to object
orientation, is an operation `draw` that can be applied to objects
representing various geometric shapes. Different implementations of
the `draw` operation can be provided for drawing circles, triangles, and
squares, and a call to `draw` will actually result in drawing a circle,
triangle, or square, depending on the type of the object to which the
`draw` operation is applied. The different implementations of `draw` are
defined separately, and new versions can be defined that draw other
shapes without having to change the code of either the caller or any
of the other `draw` implementations. This feature of object orientation
goes by the fancy Greek name *polymorphism*, meaning "many forms,"
because a single conceptual operation, such as drawing an object, can
take many different concrete forms.

面向对象的基本思想在于一种组织程序的强大方式：定义数据类型并将操作关联在那些数据类型上。特别是，你希望产生一种操作并让其确切行为取决于该操作所涉及的一个或多个对象的类型。所有关于面向对象的介绍中所使用的经典例子，是可应用于代表各种几何图形的对象的
`draw` 操作。`draw` 操作的不同实现可用于绘制圆、三角形和矩形，而对 `draw`
的调用将实际绘制出圆、三角形或矩形，具体取决于 `draw` 操作所应用到的对象类型。`draw`
的不同实现被分别定义，并且新的版本可以被定义来绘制其他图形，而无需修改调用方或是任何其他
`draw` 实现的代码。这一面向对象风格称为
“多义性”，源自希腊语 *polymorphism*，意思是
“多种形式”，因为单一的概念性操作，诸如绘制一个对象，可以带有许多不同的具体形式。

Common Lisp, like most object-oriented languages today, is
class-based; all objects are instances of a particular class. The
class of an object determines its representation--built-in classes
such as **NUMBER** and **STRING** have opaque representations accessible only
via the standard functions for manipulating those types, while
instances of user-defined classes, as you'll see in the next chapter,
consist of named parts called slots.

Common Lisp
和今天的多数面向对象语言一样都是基于类的所有对象都是某个特定类的实例。一个对象的类决定了它的表示，诸如
**NUMBER** 和 **STRING**
这样的内置类带有不透明的表示，只能通过管理这些类型的标准函数来访问，而用户自定义类的实例，如同你将在下一章里看到的，由称为槽（slot）的命名部分组成。 

Classes are arranged in a hierarchy, a taxonomy for all objects. A
class can be defined as a subclass of other classes, called its
superclasses. A class inherits part of its definition from its
superclasses and instances of a class are also considered instances of
the superclasses. In Common Lisp, the hierarchy of classes has a
single root, the class **T**, which is a direct or indirect superclass of
every other class. Thus, every datum in Common Lisp is an instance of
**T**. Common Lisp also supports multiple inheritance--a single class can
have multiple direct superclasses.

类通过层次结构组织在一起，形成了所有对象的分类系统。一个类可以定义成另一个类的子类（subclass），后者称为它的基类（superclass）。一个类从它的基类中继承（inherit）其定义的一部分，而一个类的实例也被认为是其基类的实例。在
Common Lisp 中，类的层次关系带有一个单根，即类
**T**，它是其他类的所有直接或间接基类。这样，Common Lisp
中的每一个数据都是 **T** 的一个实例。Common Lisp
也支持多继承（multiple inheritance），即单一的类可以拥有多个直接基类。

Outside the Lisp family, almost all object-oriented languages follow
the basic pattern established by Simula of having behavior associated
with classes through methods or member functions that belong to a
particular class. In these languages, a method is invoked on a
particular object, and the class of that object determines what code
runs. This model of method invocation is called--after the Smalltalk
terminology--message passing. Conceptually, method invocation in a
message-passing system starts by sending a message containing the name
of the method to run and any arguments to the object on which the
method is being invoked. The object then uses its class to look up the
method associated with the name in the message and runs it. Because
each class can have its own method for a given name, the same message,
sent to different objects, can invoke different methods.

在 Lisp 家族之外，几乎所有的面向对象语言都遵循了由 Simula
所建立的基本模式：类所关联的行为由属于一个特定类的方法（method）或成员函数（member
function）定义。在这些语言里，在一个特定对象上调用一个方法，然后该对象所属的类决定运行什么代码。这种方法调用的模型称为消息传递（message
passing），这是来自 Smalltalk
的术语。从概念上来讲，在一个消息传递系统中，方法调用开始于向被调用方法所操作的对象发送一个消息，其中含有需要运行的方法名和任何参数。该对象随后使用其类来查找与该消息中的名字所关联的方法并运行它。由于每个类对于一个给定名字都有它自己的方法，因此发送的消息到不同的对象可以调用不同的方法。

Early Lisp object systems worked in a similar way, providing a special
function SEND that could be used to send a message to a particular
object. However, this wasn't entirely satisfactory, as it made method
invocations different from normal function calls. Syntactically method
invocations were written like this:

早期的 Lisp 对象系统以类似的方式工作，提供了特殊函数
`SEND`，用于向特定对象发送一条消息。尽管如此，这种方式并不完全令人满意，因为它使得方法调用不同于正常的函数调用。句法意义上的方法调用应写成

```lisp
(send object 'foo)
```

rather than like this:

而不是下面这样：

```lisp
(foo object)
```

More significantly, because methods weren't functions, they couldn't
be passed as arguments to higher-order functions such as **MAPCAR**; if
one wanted to call a method on all the elements of a list with **MAPCAR**,
one had to write this:

更严重的是，由于方法不是函数，它们无法作为参数传递给像 **MAPCAR**
这样的高阶函数。如果一个人想要使用 **MAPCAR**
在一个列表的所有元素上调用方法，他不得不写成这样：

```lisp
(mapcar #'(lambda (object) (send object 'foo)) objects)
```

rather than this:

而不是（更自然的）这样：

```lisp
(mapcar #'foo objects)
```

Eventually the folks working on Lisp object systems unified methods
with functions by creating a new kind of function called a generic
function. In addition to solving the problems just described, generic
functions opened up new possibilities for the object system, including
many features that simply don't make sense in a message-passing object
system.

最终，设计 Lisp 对象系统上的人们通过创建一种新的称为广义函数（generic
function）的函数类型而将方法和函数统一在一起。广义函数不但解决了上面描述的问题，它还为对象系统开放了新的可能性，包括许多在消息传递对象系统中基本无法实现的特性。

Generic functions are the heart of Common Lisp's object system and the
topic of the rest of this chapter. While I can't talk about generic
functions without some mention of classes, for now I'll focus on how
to define and use generic functions. In the next chapter I'll show you
how to define your own classes.

广义函数是 Common Lisp
对象系统的核心，也是本章其余部分的主题。虽然我不可能在不提到类的情况下谈论广义函数，但目前我将把注意力集中在如何定义和使用广义函数上。在下一章里，我将向你展示如何定义你自己的类。

