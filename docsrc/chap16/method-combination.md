# Method Combination（方法组合）

Outside the body of a method, **CALL-NEXT-METHOD** has no meaning. Within
a method, it's given a meaning by the generic function machinery that
builds an effective method each time the generic function is invoked
using all the methods applicable to that particular invocation. This
notion of building an effective method by combining applicable methods
is the heart of the generic function concept and is the thing that
allows generic functions to support facilities not found in
message-passing systems. So it's worth taking a closer look at what's
really happening. Folks with the message-passing model deeply
ingrained in their consciousness should pay particular attention
because generic functions turn method dispatching inside out compared
to message passing, making the generic function, rather than the
class, the prime mover.

在一个方法体之外，**CALL-NEXT-METHOD**
没有任何意义。在一个方法之内，它被广义函数机制定义，用来在每次广义函数使用的所有应用于特定调用的方法被调用时构造一个有效方法。这种通过组合可应用的方法来构造有效方法的概念是广义函数概念的核心，并且是让广义函数可以支持消息传递系统里所没有的机制的关键。因此，值得更进一步地观察究竟发生了什么。那些在他们的意识中带有根深蒂固的消息传递模型思想的人们应当尤其注意这点，因为广义函数相比消息传递完全颠覆了方法的调度过程，使得广义函数而不是类成为了主要推动者。

Conceptually, the effective method is built in three steps: First, the
generic function builds a list of applicable methods based on the
actual arguments it was passed. Second, the list of applicable methods
is sorted according to the *specificity* of their parameter
specializers. Finally, methods are taken in order from the sorted list
and their code combined to produce the effective method.

从概念上讲，有效方法由三步构造而成：首先，广义函数基于被传递的实际参数构造一个可应用的方法列表。其次，这个可应用方法的列表按照它们的参数特化符中的特化程度（specificity）排序。最后，根据排序后列表中的顺序来取出这些方法并将它们的代码组合起来以产生有效方法。

To find applicable methods, the generic function compares the actual
arguments with the corresponding parameter specializers in each of its
methods. A method is applicable if, and only if, all the specializers
are compatible with the corresponding arguments.

为了找出可应用的方法，广义函数将实际参数与它的每一个方法中的对应参数特化符进行比较。当且仅当所有特化符均和对应的参数兼容，一个方法便是可应用的。

When the specializer is the name of a class, it's compatible if it
names the actual class of the argument or one of its
superclasses. (Recall that parameters without explicit specializers
are implicitly specialized on the class **T** so will be compatible with
any argument.) An **EQL** specializer is compatible only when the argument
is the same object as was specified in the specializer.

当特化符是一个类的名字时，如果该名字是参数的实际类名或是它的一个基类的名字，那么该特化符将是兼容的。（再次强调，不带有显式特化符的形参将隐式特化到类
**T** 上从而与任何参数兼容。）一个
**EQL** 特化符当且仅当参数和特化符中所指定的对象是同一个时才是兼容的。

Because all the arguments are checked against the corresponding
specializers, they all affect whether a method is applicable. Methods
that explicitly specialize more than one parameter are called
*multimethods*; I'll discuss them in the section "Multimethods."

由于所有参数都将在对应的特化符中被检查，它们都会影响一个方法是否是可应用的。显式地特化了超过一个形参的方法被称为多重方法（multimethod）。我将在
“多重方法” (16.8） 节中讨论它们。

After the applicable methods have been found, the generic function
machinery needs to sort them before it can combine them into an
effective method. To order two applicable methods, the generic
function compares their parameter specializers from left to right,10
and the first specializer that's different between the two methods
determines their ordering, with the method with the more specific
specializer coming first.

在可应用的方法被找到以后，广义函数机制需要在将它们组合成一个有效方法之前对它们进行排序。为了确定两个可应用方法的顺序，广义函数从左到右比较它们的参数特化符， 并且两个方法中第一个不同的特化符将决定它们的顺序，其中带有更加特定的特化符的方法排在前面。

Because only applicable methods are being sorted, you know all class
specializers will name classes that the corresponding argument is
actually an instance of. In the typical case, if two class
specializers differ, one will be a subclass of the other. In that
case, the specializer naming the subclass is considered more
specific. This is why the method that specialized `account` on
`checking-account` was considered more specific than the method that
specialized it on `bank-account`.

由于只有可应用的方法在排序，你可以看出所有由类特化符命名的类对应的参数实际上都是它们的实例。在典型情况下，如果两个类特化符不同，那么其中一个将是另一个的子类。在那种情况下，命名了子类的特化符将被认为是更加相关的。这就是为什么在
`checking-account` 上特化了 `account`
的方法被认为比在 `bank-account` 上特化它的方法是更加相关的。

Multiple inheritance slightly complicates the notion of specificity
since the actual argument may be an instance of two classes, neither
of which is a subclass of the other. If such classes are used as
parameter specializers, the generic function can't order them using
only the rule that subclasses are more specific than their
superclasses. In the next chapter I'll discuss how the notion of
specificity is extended to deal with multiple inheritance. For now,
suffice it to say that there's a deterministic algorithm for ordering
class specializers.

多重继承稍微复杂化了特化性的概念，因为实际参数可能是两个类的实例，而两者都不是对方的子类。如果这样的类被用于参数特化符，那么广义函数就无法只通过子类比它们的基类更加相关这一规则来决定它们的顺序。在下一章里，我将讨论特化性的概念如何被扩展用于处理多重继承。目前我要说明的只是存在一个确定的算法来决定类特化符的顺序。

Finally, an **EQL** specializer is always more specific than any class
specializer, and because only applicable methods are being considered,
if more than one method has an **EQL** specializer for a particular
parameter, they must all have the same **EQL** specializer. The comparison
of those methods will thus be decided based on other parameters.

最后， **EQL**
特化符总是比任何类特化符更加相关，并且由于只有可应用的方法被考虑，如果对于一个特定形参有多于一个方法带有
**EQL** 特化符，那么它们一定全部带有相同的
**EQL** 特化符。这样对这些方法的比较将取决于其他参数。
