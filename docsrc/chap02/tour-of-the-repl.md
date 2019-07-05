# Lather, Rinse, Repeat: A Tour of the REPL （REPL简介）

In this chapter you'll set up your programming environment and write
your first Common Lisp programs. We'll use the easy-to-install Lisp in
a Box developed by Matthew Danish and Mikel Evins, which packages a
Common Lisp implementation with Emacs, a powerful Lisp-aware text
editor, and SLIME,1 a Common Lisp development environment built on top
of Emacs.

本章将学习如何设置编程环境并编写第一个 Common Lisp 程序。我们将使用由
Matthew Danish 和 Mikel Evins 开发的安装便利的 Lisp-in-a-Box
环境，它封装了带有 Emacs 强大且对 Lisp 较为友好的文本编辑器的 Common
Lisp 实现，以及 SLIME（Superior Lisp Interaction Mode for Emacs）——构建在
Emacs 之上的 Common Lisp 开发环境。

This combo provides a state-of-the-art Common Lisp development
environment that supports the incremental, interactive development
style that characterizes Lisp programming. The SLIME environment has
the added advantage of providing a fairly uniform user interface
regardless of the operating system and Common Lisp implementation you
choose. I'll use the Lisp in a Box environment in order to have a
specific development environment to talk about; folks who want to
explore other development environments such as the graphical
integrated development environments (IDEs) provided by some of the
commercial Lisp vendors or environments based on other editors
shouldn't have too much trouble translating the basics.

上述组合提供了一个全新的 Common Lisp
开发环境，可以支持增量、交互式的开发风格——这是 Lisp 编程所特有的。SLIME
环境提供了一个完全统一的用户接口，跟你所选择的操作系统和 Common
Lisp 实现无关。为了能有一个具体的开发环境来进行讲解，我将使用这个
Lisp-in-a-Box 环境。而那些想要其他开发环境的人，无论是使用某些商业
Lisp 供应商的图形化集成开发环境（IDE）还是基于其他编辑器的环境，在应用本书的内容时应该都不会有较大的困难。
