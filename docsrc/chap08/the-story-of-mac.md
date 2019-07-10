# The Story of Mac: A Just-So Story（Mac 的故事：只是一个故事）

Once upon a time, long ago, there was a company of Lisp
programmers. It was so long ago, in fact, that Lisp had no
macros. Anything that couldn't be defined with a function or done with
a special operator had to be written in full every time, which was
rather a drag. Unfortunately, the programmers in this company--though
brilliant--were also quite lazy. Often in the middle of their
programs--when the tedium of writing a bunch of code got to be too
much--they would instead write a note describing the code they needed
to write at that place in the program. Even more unfortunately,
because they were lazy, the programmers also hated to go back and
actually write the code described by the notes. Soon the company had a
big stack of programs that nobody could run because they were full of
notes about code that still needed to be written.

很久以前，有一个由 Lisp
程序员们所组成的公司。由于年代过于久远，所以那个时候的 Lisp
还没有宏。每次，任何不能用函数来定义或是用特殊操作符来完成的事情都不得不通过手写来实现，这带来了很大的不便。不幸的是，这个公司的程序员们虽然杰出但却非常懒惰。在他们的程序中，当需要编写大量单调乏味的代码时，他们往往会写下一个注释来描述想要在该位置上编写的代码。更不幸的是，由于很懒惰，他们也很讨厌回过头去实际编写那些注释所描述的代码。不久，这个公司就有了一大堆无人可以运行的程序，因为全都是代表着尚需编写的代码的注释。

In desperation, the big bosses hired a junior programmer, Mac, whose
job was to find the notes, write the required code, and insert it into
the program in place of the notes. Mac never ran the programs--they
weren't done yet, of course, so he couldn't. But even if they had been
completed, Mac wouldn't have known what inputs to feed them. So he
just wrote his code based on the contents of the notes and sent it
back to the original programmer.

走投无路之下，老板雇用了一个初级程序员
Mac。他的工作就是找到这些注释，编写所需的代码，然后再将其替换掉程序中的注释。Mac
从未运行过这些程序——因为程序尚未完成，所以他当然运行不了。但就算这些程序完成了，Mac
也不知道该用怎样的输入来运行它们。因此，他只是基于注释的内容来编写他的代码，再将其发还给最初的程序员。

With Mac's help, all the programs were soon completed, and the company
made a ton of money selling them--so much money that the company could
double the size of its programming staff. But for some reason no one
thought to hire anyone to help Mac; soon he was single- handedly
assisting several dozen programmers. To avoid spending all his time
searching for notes in source code, Mac made a small modification to
the compiler the programmers used. Thereafter, whenever the compiler
hit a note, it would e-mail him the note and wait for him to e-mail
back the replacement code. Unfortunately, even with this change, Mac
had a hard time keeping up with the programmers. He worked as
carefully as he could, but sometimes-- especially when the notes
weren't clear--he would make mistakes.

在 Mac
的帮助下，不久所有的程序都完成了，公司通过销售它们赚了很多钱，并用这些钱将其程序员团队扩大了一倍。但不知为何，没有人想到雇用任何人来帮助
Mac。很快他就开始单枪匹马地同时协助几十个程序员了。为了避免将他所有的时间都花在搜索源代码的注释上，Mac
对程序员们使用的编译器做了一个小小的更改。从那以后，只要编译器遇到一个注释，它就会将注释以电子邮件的形式发给他并等待他将替换的代码传送回来。然而，就算有了这个变化，Mac
也很难跟上程序员的进度。他尽可能小心地工作，但有时，尤其是当注释不够清楚时，他会犯错误。

The programmers noticed, however, that the more precisely they wrote
their notes, the more likely it was that Mac would send back correct
code. One day, one of the programmers, having a hard time describing
in words the code he wanted, included in one of his notes a Lisp
program that would generate the code he wanted. That was fine by Mac;
he just ran the program and sent the result to the compiler.

不过程序员们注意到了，他们将注释写得越精确，Mac
就越有可能发回正确的代码。一天，一个花费大量时间用文字来描述他想要的代码未遂的程序员，在他的注释里写入了一个可以生成他想要的代码的
Lisp 程序。这对 Mac
来说很简单；他只需运行这个程序并将结果发给编译器就好了。

The next innovation came when a programmer put a note at the top of
one of his programs containing a function definition and a comment
that said, "Mac, don't write any code here, but keep this function for
later; I'm going to use it in some of my other notes." Other notes in
the same program said things such as, "Mac, replace this note with the
result of running that other function with the symbols `x` and `y` as
arguments."

接下来又出现了一种创新。有一个程序员在他程序的开始处写了一段备注，其中含有一个函数定义以及另一个注释，该注释为：“Mac，不要在这里写任何代码，但要把这个函数留给以后使用，我将在我的其他一些注释里用到它。”
同一个程序里的还有如下这样描述的注释：“Mac，将这个注释替换成用符号 `x`
和 `y` 作为参数来运行上面提到的那个函数所得到的结果。”

This technique caught on so quickly that within a few days, most
programs contained dozens of notes defining functions that were only
used by code in other notes. To make it easy for Mac to pick out the
notes containing only definitions that didn't require any immediate
response, the programmers tagged them with the standard preface:
"Definition for Mac, Read Only." This--as the programmers were still
quite lazy--was quickly shortened to "DEF. MAC. R/O" and then
"**DEFMACRO**."

这项技术在几天里就迅速流行起来，多数程序都含有数十个注释，它们定义了那些只被其他注释中的代码所使用的函数。为了使
Mac 更容易地辨别那些只含有定义而不必立即回复的注释，程序员们用一个标准
前缀来标记它们：“给
Mac 的定义，仅供阅读。”（Definition for Mac, Read
Only.）由于程序员们仍然很懒惰，这个写法很快被简化成
“DEF.MAC. R/O”，接着又被简化为 “**DEFMACRO**”。

Pretty soon, there was no actual English left in the notes for
Mac. All he did all day was read and respond to e-mails from the
compiler containing **DEFMACRO** notes and calls to the functions defined
in the **DEFMACRO**s. Since the Lisp programs in the notes did all the
real work, keeping up with the e-mails was no problem. Mac suddenly
had a lot of time on his hands and would sit in his office daydreaming
about white-sand beaches, clear blue ocean water, and drinks with
little paper umbrellas in them.

不久以后，这些给 Mac 的注释中再没有实际可读的英语了。Mac
每天要做的事情就是阅读并反馈那些来自编译器的含有 **DEFMACRO**
注释的电子邮件以及调用那些 **DEFMACRO**
里所定义的函数。由于注释中的 Lisp
程序做了所有实际的工作，跟上这些电子邮件的进度完全没有问题。Mac
手头上突然有了大量时间，可以坐在他的办公室里做那些关于白色沙滩、蓝色海水和鸡尾酒的白日梦了。

Several months later the programmers realized nobody had seen Mac for
quite some time. When they went to his office, they found a thin layer
of dust over everything, a desk littered with travel brochures for
various tropical locations, and the computer off. But the compiler
still worked--how could it be? It turned out Mac had made one last
change to the compiler: instead of e-mailing notes to Mac, the
compiler now saved the functions defined by **DEFMACRO** notes and ran
them when called for by the other notes. The programmers decided there
was no reason to tell the big bosses Mac wasn't coming to the office
anymore. So to this day, Mac draws a salary and from time to time
sends the programmers a postcard from one tropical locale or another.

几个月以后，程序员们意识到已经很长时间没人见过 Mac
了。当他们去他的办公室时，发现所有东西上都积了薄薄的一层灰，一个桌子上还放着几本热带地区的旅行手册，而电脑则是关着的。但是编译器却仍在正常工作——这怎么可能？看起来
Mac 对编译器做了最后一个修改：现在不需要用电子邮件将注释发给 Mac
了，而是编译器会将那些 **DEFMACRO**
中所定义的函数保存下来，并在其被其他注释调用时运行它们。程序员们觉得没有理由告诉老板
Mac 不再来办公室了。因此直到今天，Mac
还领着薪水，并且时不时地会从某个热带地区给程序员们发一张明信片。
