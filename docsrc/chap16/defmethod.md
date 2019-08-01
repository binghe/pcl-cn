# DEFMETHOD

Now you're ready to use **DEFMETHOD** to define methods that implement
withdraw.

现在你开始使用 **DEFMETHOD** 来定义实现了 `withdraw` 的方法。

A method's parameter list must be congruent with its generic
function's. In this case, that means all methods defined on `withdraw`
must have exactly two required parameters. More generally, methods
must have the same number of required and optional parameters and must
be capable of accepting any arguments corresponding to any `&rest` or
`&key` parameters specified by the generic function.

方法的形参列表必须与它的广义函数保持一致。在本例中，这意味着所有定义在 `withdraw`
上的方法都必须刚好有两个必要参数。在更一般的情况下，方法必须带有由广义函数指定的相同数量的必要和可选参数，并且必须可以接受对应于任何
`&rest` 或 `&key` 形参的参数。

Since the basics of withdrawing are the same for all accounts, you can
define a method that specializes the account parameter on the
`bank-account` class. You can assume the function balance returns the
current balance of the account and can be used with **SETF**--and thus
with **DECF**--to set the balance. The function **ERROR** is a standard
function used to signal an error, which I'll discuss in greater detail
in Chapter 19. Using those two functions, you can define a basic
withdraw method that looks like this:

由于提款的基本操作对于所有帐户都是相同的，因此你可以定义一个方法，其在
`bank-account` 类上特化了 `account` 参数。你可以假设函数
`balance` 返回当前帐户的余额并且可被用于同 **SETF**（因此也包括
**DECF**）一起来设置余额。函数 **ERROR**
是一个用于报错的标准函数，我将在第 19
章里讨论其进一步的细节。使用这两个函数，你可以像下面这样定义出一个基本的
`withdraw` 方法：

```lisp
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))
```

As this code suggests, the form of **DEFMETHOD** is even more like that of
**DEFUN** than **DEFGENERIC**'s is. The only difference is that the required
parameters can be specialized by replacing the parameter name with a
two-element list. The first element is the name of the parameter, and
the second element is the specializer, either the name of a class or
an **EQL** specializer, the form of which I'll discuss in a moment. The
parameter name can be anything--it doesn't have to match the name used
in the generic function, though it often will.

如同这段代码显示的，**DEFMETHOD** 的形式比 **DEFGENERIC**
更像是一个 **DEFUN**
形式。唯一的区别在于必要形参可以通过将形参名替换成两元素列表来进行特化。其中第一个元素是形参名，而第二个元素是特化符，其要么是类的名字要么是
**EQL**
特化符，其形式我将很快讨论到。形参名可以是任何东西──它不需要匹配广义函数中使用的名字，尽管经常是使用相同的名字。

This method will apply whenever the first argument to `withdraw` is an
instance of `bank-account`. The second parameter, `amount`, is implicitly
specialized on **T**, and since all objects are instances of **T**, it doesn't
affect the applicability of the method.

该方法在每当 `withdraw` 的第一个参数是 `bank-account`
的实例时被应用。第二个形参 `amount`
被隐式特化到 **T** 上，而由于所有对象都是T的实例，它不会影响该方法的可应用性。

Now suppose all checking accounts have overdraft protection. That is,
each checking account is linked to another bank account that's drawn
upon when the balance of the checking account itself can't cover a
withdrawal. You can assume that the function `overdraft-account` takes a
`checking-account` object and returns a `bank-account` object representing
the linked account.

现在假设所有现金帐户都带有透支保护。这就是说，每个现金帐户都与另一个银行帐户相关联，该帐户将在现金帐户的余额本身无法满足提款需求时被提款。你可以假设函数
`overdraft-account` 接受 `checking-account`
对象并返回代表了关联帐户的 `bank-account` 对象。

Thus, withdrawing from a `checking-account` object requires a few extra
steps compared to withdrawing from a standard `bank-account` object. You
must first check whether the amount being withdrawn is greater than
the account's current balance and, if it is, transfer the difference
from the overdraft account. Then you can proceed as with a standard
`bank-account` object.

这样，相比从标准的 `bank-account` 对象中提款，从 `checking-account`
对象中提款需要一些额外的步骤。你必须首先检查提款金额是否大于该帐户的当前余额，如果大于，就将差额转给透支帐户。然后你可以像处理标准的
`bank-account` 对象那样进行处理。

So what you'd like to do is define a method on `withdraw` that
specializes on `checking-account` to handle the transfer and then lets
the method specialized on `bank-account` take control. Such a method
might look like this:

因此，你要做的是在 `withdraw`
上定义一个特化在 `checking-account`
上的方法来处理该传递过程，然后再让特化在 `bank-account`
上的方法接手。这样一个方法如下所示：

```lisp
(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))
```

The function **CALL-NEXT-METHOD** is part of the generic function
machinery used to combine applicable methods. It indicates that
control should be passed from this method to the method specialized on
`bank-account`. When it's called with no arguments, as it is here, the
next method is invoked with whatever arguments were originally passed
to the generic function. It can also be called with arguments, which
will then be passed onto the next method.

函数 **CALL-NEXT-METHOD**
是广义函数机制的一部分，用于组合可应用的方法。它指示控制应当从该方法传递到特定于
`bank-account` 的方法上。 当它不带参数被调用时，就像这里的这样，下一个方法将以最初传递给广义函数的参数被调用。它也可以带参数被调用，这些参数随后被传递给下一个方法。

You aren't required to invoke **CALL-NEXT-METHOD** in every
method. However, if you don't, the new method is then responsible for
completely implementing the desired behavior of the generic
function. For example, if you had a subclass of `bank-account`,
`proxy-account`, that didn't actually keep track of its own balance but
instead delegated withdrawals to another account, you might write a
method like this (assuming a function, `proxied-account`, that returns
the proxied account):

你不必在每一个方法中调用
**CALL-NEXT-METHOD**。尽管如此，如果你不这样做的话，新的方法将负责完全你想要的广义函数行为。假如你有一个
`bank-account` 的子类
`proxy-account`，它并不实际跟踪自己的余额而是将提款请求代理到其他帐户，那么你可以写一个如下的方法（假设有一个函数
`proxied-account` 的方法可以返回代理的帐户）：

```lisp
(defmethod withdraw ((proxy proxy-account) amount)
  (withdraw (proxied-account proxy) amount))
```

Finally, **DEFMETHOD** also allows you to create methods specialized on a
particular object with an **EQL** specializer. For example, suppose the
banking app is going to be deployed in a particularly corrupt
bank. Suppose the variable `*account-of-bank-president*` holds a
reference to a particular bank account that belongs--as the name
suggests--to the bank's president. Further suppose the variable `*bank*`
represents the bank as a whole, and the function `embezzle` steals money
from the bank. The bank president might ask you to "fix" withdraw to
handle his account specially.

最后，**DEFMETHOD**
还允许你通过使用 **EQL**
特化符来创建特化在一个特定对象上的方法。例如，假设该银行应用被部署在某个腐败银行上。假设变量
`*account-of-bank-president*` 保存了一个特定银行帐户的引用，如同其名字显示的，该帐户属于该银行的总裁。进一步假设变量
`*bank*` 代表该银行整体，而函数 `embezzle`
可以从银行中偷钱。银行总裁可能会让你 “修复” `withdraw` 来特别处理他的帐户。

```lisp
(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
  (call-next-method)))
```

Note, however, that the form in the **EQL** specializer that provides the
object to specialize on--`*account-of-bank-president*` in this case--is
evaluated once, when the **DEFMETHOD** is evaluated. This method will be
specialized on the value of `*account-of-bank-president*` at the time
the method is defined; changing the variable later won't change the
method.

不过需要注意到，**EQL** 特化符中提供了特化对象的形式，在本例中是变量
`*account-of-bank-president*`，其只在 **DEFMETHOD**
被求值时求值一次。在定义方法时该方法将特化
`*account-of-bank-president*` 的值。随后，改变该变量将不会改变该方法。

