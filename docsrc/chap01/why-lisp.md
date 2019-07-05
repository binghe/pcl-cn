# Why Lisp?（为什么是 Lisp？）

It's hard, in only a few pages of an introductory chapter, to explain
why users of a language like it, and it's even harder to make the case
for why you should invest your time in learning a certain
language. Personal history only gets us so far. Perhaps I like Lisp
because of some quirk in the way my brain is wired. It could even be
genetic, since my dad has it too. So before you dive into learning
Lisp, it's reasonable to want to know what the payoff is going to be.

很难用绪言这一章的寥寥数页来说清楚为何一门语言的用户会喜欢这门语言，更难的是找出一个理由说服你花时间来学习某种语言。现身说法只能到此为止了。也许我喜欢
Lisp 是因为它刚好符合我的思维方式，甚至可能是遗传因素，因为我父亲也喜欢它。因此在你开始学习
Lisp 之前就想知道会得到什么回报也是合理的。

For some languages, the payoff is relatively obvious. For instance, if
you want to write low-level code on Unix, you should learn C. Or if
you want to write certain kinds of cross-platform applications, you
should learn Java. And any of a number companies still use a lot of
C++, so if you want to get a job at one of them, you should learn C++.

对某些语言来说，回报是相对明显的。举个例子，如果打算编写 Unix
下的底层代码，那你应该去学
C。或者如果打算编写特定的跨平台应用程序，那你应该去学
Java。相当数量的公司仍在大量使用
C++，所以如果你打算在这些公司里找到工作，那就应该去学 C++。

For most languages, however, the payoff isn't so easily categorized;
it has to do with subjective criteria such as how it feels to use the
language. Perl advocates like to say that Perl "makes easy things easy
and hard things possible" and revel in the fact that, as the Perl
motto has it, "There's more than one way to do it."1 Python's fans, on
the other hand, think Python is clean and simple and think Python code
is easier to understand because, as their motto says, "There's only
one way to do it."

尽管如此，对于多数语言来说，回报往往不是那么容易界定的，这里面存在包括个人感情在内的主观因素。Perl
拥趸们经常说 Perl “让简单的事情简单，让复杂的事情可行”，并且沉迷于
“做事情永远都有不止一种方法” 这一 Perl 格言所推崇的事实。另一方面，Python
爱好者们认为 Python 是简洁的，并且 Python
代码之所以易于理解，是因为正如他们的格言所说：“做每件事只有一种方法。”

So, why Common Lisp? There's no immediately obvious payoff for
adopting Common Lisp the way there is for C, Java, and C++ (unless, of
course, you happen to own a Lisp Machine). The benefits of using Lisp
have much more to do with the experience of using it. I'll spend the
rest of this book showing you the specific features of Common Lisp and
how to use them so you can see for yourself what it's like. For now
I'll try to give you a sense of Lisp's philosophy.

那么 Common Lisp 呢？没有迹象表明采用 Common Lisp 可以立即得到诸如
C、Java 和 C++ 那样显而易见的好处（当然了，除非刚好拥有一台 Lisp 机）。使用
Lisp 所获得的好处在很大程度上取决于使用经验。本书的其余部分将向读者展示
Common Lisp 的特性以及如何使用它们，让你自己去感受它。眼下，我只想让你先对
Lisp 哲学有个大致的感受。

The nearest thing Common Lisp has to a motto is the koan-like
description, "the programmable programming language." While cryptic,
that description gets at the root of the biggest advantage Common Lisp
still has over other languages. More than any other language, Common
Lisp follows the philosophy that what's good for the language's
designer is good for the language's users. Thus, when you're
programming in Common Lisp, you almost never find yourself wishing the
language supported some feature that would make your program easier to
write, because, as you'll see throughout this book, you can just add
the feature yourself.

Common Lisp 中最接近格言的是一句类似禅语的描述：“可编程的编程语言。”
虽然隐晦了一些，但这句话却道出了 Common Lisp
至今仍然雄踞其他语言之上的最大优势。Common Lisp
比其他任何语言都更加遵循一种哲学——凡利于语言设计者的也利于语言使用者。这就意味着，当使用
Common Lisp
编程的时候，你永远不会遇到这种情况：语言里刚好缺乏某些可能令程序更容易编写的特性，因为正如你将在本书中看到的你可以为语言添加任何想要的特性。

Consequently, a Common Lisp program tends to provide a much clearer
mapping between your ideas about how the program works and the code
you actually write. Your ideas aren't obscured by boilerplate code and
endlessly repeated idioms. This makes your code easier to maintain
because you don't have to wade through reams of code every time you
need to make a change. Even systemic changes to a program's behavior
can often be achieved with relatively small changes to the actual
code. This also means you'll develop code more quickly; there's less
code to write, and you don't waste time thrashing around trying to
find a clean way to express yourself within the limitations of the
language.

因此，Common Lisp
程序倾向于把关于程序如何工作的想法更清楚地映射到实际所写的代码上。想法永远不会被过于紧凑的代码和不断重复的模式搞得含糊不清。这将使代码更加易于维护，因为不必在每次修改之前都先复查大量的相关代码。甚至对程序行为的系统化调整也经常可以通过对实际代码作相对少量的修改来实现。这也意味着可以更快速地开发，编写更少的代码，也不必再花时间在语言的限制里寻求更简洁的方式来表达想法了。

Common Lisp is also an excellent language for exploratory
programming--if you don't know exactly how your program is going to
work when you first sit down to write it, Common Lisp provides several
features to help you develop your code incrementally and
interactively.

Common Lisp 也是一门适合做探索性编程的优秀语言。如果在刚开始编写程序的
时候对整个工作机制还不甚明了，Common Lisp
提供了一些特性可以有助于实现递进的交互式开发。

For starters, the interactive read-eval-print loop, which I'll
introduce in the next chapter, lets you continually interact with your
program as you develop it. Write a new function. Test it. Change
it. Try a different approach. You never have to stop for a lengthy
compilation cycle.

对于初学者来说，将在下一章介绍的交互式 “读—求值—打印”
循环可以让你在开发过程中持续地与程序交互。编写新函数、测试它、修改它、尝试不同的实现方法。从而使思路不会因漫长的编译周期而停滞下来。

Other features that support a flowing, interactive programming style
are Lisp's dynamic typing and the Common Lisp condition
system. Because of the former, you spend less time convincing the
compiler you should be allowed to run your code and more time actually
running it and working on it,4 and the latter lets you develop even
your error handling code interactively.

其他支持连贯的交互式编程风格的语言特性，还包括 Lisp
的动态类型机制以及
Common Lisp 状态系统（condition system）。由于前者的存在，只需花较少的时间就能让编译器把代码跑起来，然后把更多的时间放在如何实际运行和改进代码上，而后者甚至可以实现交互式地开发错误处理代码。

Another consequence of being "a programmable programming language" is
that Common Lisp, in addition to incorporating small changes that make
particular programs easier to write, can easily adopt big new ideas
about how programming languages should work. For instance, the
original implementation of the Common Lisp Object System (CLOS),
Common Lisp's powerful object system, was as a library written in
portable Common Lisp. This allowed Lisp programmers to gain actual
experience with the facilities it provided before it was officially
incorporated into the language.

作为一门 “可编程的编程语言”，Common Lisp
除了支持各种小修小补以便开发人员更容易地编写某些程序之外，对于那些从根本上改变编程方式的新思想的支持也是绰绰有余的。例如，Common
Lisp 强大的对象系统 CLOS（Common Lisp Object System），其最初的实现就是一个用可移植的
Common Lisp 写成的库。而在这个库正式成为语言的一部分之前，Lisp
程序员就可以体验其实际功能了。

Whatever new paradigm comes down the pike next, it's extremely likely
that Common Lisp will be able to absorb it without requiring any
changes to the core language. For example, a Lisper has recently
written a library, AspectL, that adds support for aspect-oriented
programming (AOP) to Common Lisp.5 If AOP turns out to be the next big
thing, Common Lisp will be able to support it without any changes to
the base language and without extra preprocessors and extra
compilers.

目前来看，无论下一个流行的编程范例是什么，Common Lisp
都极有可能在无需修改其语言核心部分的情况下将其吸纳进来。例如，最近有个
Lisp 程序员写了一个叫做 AspectL 的库，它为 Common Lisp
增加了对面向方面编程（AOP）的支持。如果 AOP 将主宰编程的未来，那么 Common Lisp
将可以直接支持它而无需对基础语言作任何修改，并且也不需要额外的预处理器和编译器。
