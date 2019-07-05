# Practical: A Simple Database（实践：一个简单的数据库）

Obviously, before you can start building real software in Lisp, you'll
have to learn the language. But let's face it--you may be thinking,
"'Practical Common Lisp,' isn't that an oxymoron? Why should you be
expected to bother learning all the details of a language unless it's
actually good for something you care about?" So I'll start by giving
you a small example of what you can do with Common Lisp. In this
chapter you'll write a simple database for keeping track of
CDs. You'll use similar techniques in Chapter 27 when you build a
database of MP3s for our streaming MP3 server. In fact, you could
think of this as part of the MP3 software project--after all, in order
to have a bunch of MP3s to listen to, it might be helpful to be able
to keep track of which CDs you have and which ones you need to rip.

很明显，在你可以用 Lisp
构建真实软件之前，必须先学会这门语言。但是请想想看——你可能会觉得：“‘实用
Common Lisp
编程’ 难道不是反语吗？难道在确定一门语言真正有用之前就要先把它所有的细节都学完吗？”
因此我先给你一个小型的可以用 Common Lisp 来做的例子。本章里编写一个简单的数据库用来记录
CD 光盘。在第 27 章里，为我们的流式 MP3 服务器构建一个 MP3
数据库还会用到类似的技术。事实上，它可以被看成是整个 MP3
软件项目的一部分——毕竟，为了有大量的 MP3 可听，对我们所拥有并需要转换成
MP3 的 CD 加以记录是很有用的。

In this chapter, I'll cover just enough Lisp as we go along for you to
understand how the code works. But I'll gloss over quite a few
details. For now you needn't sweat the small stuff--the next several
chapters will cover all the Common Lisp constructs used here, and
more, in a much more systematic way.

在本章，我只介绍足以使你理解代码工作原理的那些
Lisp 特性，但细节方面不会解释太多。目前你不需要执著于细节——接下来的几章将以一种更加系统化的方式介绍这里用到的所有
Common Lisp 控制结构以及更多内容。

One terminology note: I'll discuss a handful of Lisp operators in this
chapter. In Chapter 4, you'll learn that Common Lisp provides three
distinct kinds of operators: functions, macros, and special
operators. For the purposes of this chapter, you don't really need to
know the difference. I will, however, refer to different operators as
functions or macros or special operators as appropriate, rather than
trying to hide the details behind the word operator. For now you can
treat function, macro, and special operator as all more or less
equivalent.

关于术语方面，本章将讨论少量 Lisp 操作符。第4章将学到 Common Lisp
所提供的三种不同类型的操作符：函数、宏，以及特殊操作符。对于本章来说，你并不需要知道它们的区别。尽管如此，在提及操作符时我还是会适时地说成是函数、宏或特殊操作符，而不会笼统地用“操作符”这个词来表示。眼下你差不多可以认为函数、宏和特殊操作符是等价的。

Also, keep in mind that I won't bust out all the most sophisticated
Common Lisp techniques for your very first post-"hello, world"
program. The point of this chapter isn't that this is how you would
write a database in Lisp; rather, the point is for you to get an idea
of what programming in Lisp is like and to see how even a relatively
simple Lisp program can be quite featureful.

另外请记住我不会在这个继 “hello, world” 后写的首个程序中亮出所有最专业的
Common Lisp 技术来。本章的重点和意图也不在于讲解如何用 Lisp
编写数据库，而在于让你对 Lisp 编程有个大致的印象，并能看到即便相对简单的
Lisp 程序也可以有着丰富的功能。
