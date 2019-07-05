# Why Lisp? 为什么是 Lisp？

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

For most languages, however, the payoff isn't so easily categorized;
it has to do with subjective criteria such as how it feels to use the
language. Perl advocates like to say that Perl "makes easy things easy
and hard things possible" and revel in the fact that, as the Perl
motto has it, "There's more than one way to do it."1 Python's fans, on
the other hand, think Python is clean and simple and think Python code
is easier to understand because, as their motto says, "There's only
one way to do it."

So, why Common Lisp? There's no immediately obvious payoff for
adopting Common Lisp the way there is for C, Java, and C++ (unless, of
course, you happen to own a Lisp Machine). The benefits of using Lisp
have much more to do with the experience of using it. I'll spend the
rest of this book showing you the specific features of Common Lisp and
how to use them so you can see for yourself what it's like. For now
I'll try to give you a sense of Lisp's philosophy.

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

Common Lisp is also an excellent language for exploratory
programming--if you don't know exactly how your program is going to
work when you first sit down to write it, Common Lisp provides several
features to help you develop your code incrementally and
interactively.

For starters, the interactive read-eval-print loop, which I'll
introduce in the next chapter, lets you continually interact with your
program as you develop it. Write a new function. Test it. Change
it. Try a different approach. You never have to stop for a lengthy
compilation cycle.

Other features that support a flowing, interactive programming style
are Lisp's dynamic typing and the Common Lisp condition
system. Because of the former, you spend less time convincing the
compiler you should be allowed to run your code and more time actually
running it and working on it,4 and the latter lets you develop even
your error handling code interactively.

Another consequence of being "a programmable programming language" is
that Common Lisp, in addition to incorporating small changes that make
particular programs easier to write, can easily adopt big new ideas
about how programming languages should work. For instance, the
original implementation of the Common Lisp Object System (CLOS),
Common Lisp's powerful object system, was as a library written in
portable Common Lisp. This allowed Lisp programmers to gain actual
experience with the facilities it provided before it was officially
incorporated into the language.

Whatever new paradigm comes down the pike next, it's extremely likely
that Common Lisp will be able to absorb it without requiring any
changes to the core language. For example, a Lisper has recently
written a library, AspectL, that adds support for aspect-oriented
programming (AOP) to Common Lisp.5 If AOP turns out to be the next big
thing, Common Lisp will be able to support it without any changes to
the base language and without extra preprocessors and extra
compilers.
