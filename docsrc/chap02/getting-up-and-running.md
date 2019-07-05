# Getting Up and Running with Lisp in a Box（安装和运行 Lisp-in-a-Box）

Since the Lisp in a Box packaging is designed to get new Lispers up
and running in a first-rate Lisp development environment with minimum
hassle, all you need to do to get it running is to grab the
appropriate package for your operating system and the preferred Lisp
from the Lisp in a Box Web site listed in Chapter 32 and then follow
the installation instructions.

由于 Lisp-in-a-Box 软件包可以让新 Lisp 程序员在一流的 Lisp
开发环境上近乎无痛起步，因此你需要做的就是根据你的操作系统和喜爱的 Lisp
平台从第 32 章中列出的 Lisp-in-a-Box 的 Web
站点上获取相应的安装包，然后按照安装说明提示来操作即可。

Since Lisp in a Box uses Emacs as its editor, you'll need to know at
least a bit about how to use it. Perhaps the best way to get started
with Emacs is to work through its built-in tutorial. To start the
tutorial, select the first item of the Help menu, Emacs tutorial. Or
press the Ctrl key, type h, release the Ctrl key, and then press
t. Most Emacs commands are accessible via such key combinations;
because key combinations are so common, Emacs users have a notation
for describing key combinations that avoids having to constantly write
out combinations such as "Press the Ctrl key, type h, release the Ctrl
key, and then press t." Keys to be pressed together--a so-called key
chord--are written together and separated by a hyphen. Keys, or key
chords, to be pressed in sequence are separated by spaces. In a key
chord, C represents the Ctrl key and M represents the Meta key (also
known as Alt). Thus, we could write the key combination we just
described that starts the tutorial like so: C-h t.

由于 Lisp-in-a-Box 使用 Emacs 作为其编辑器，因此你至少得懂一点儿它的使用方法。最好的
Emacs 入门方法也许就是跟着它内置的向导走一遍。要想启动这个向导，选择帮助菜单的第一项
Emacs tutorial 即可。或者按住 Ctrl 键，输入 `h`，然后放开 Ctrl 键，再按
`t`。大多数 Emacs 命令都可以通过类似的组合键来访问。由于组合键用得如此普遍，因而
Emacs用户使用了一种记号来描述组合键，以避免经常书写诸如 “按住 Ctrl 键，输入
`h`，然后放开 Ctrl 键，再按 `t`”
这样的组合。需要一起按的键即所谓的键和弦，用连接号（`-`）连接；顺序按
下的键（或者键和弦键）用空格分隔。在一个键和弦里，`C` 代表 Ctrl 键而
`M` 代表 Meta 键（也就是 Alt 键）。这样，我们可以将刚才描述的启动向导的按键组合直接写成：`C-h t`。

The tutorial describes other useful commands and the key combinations
that invoke them. Emacs also comes with extensive online documentation
using its own built-in hypertext documentation browser, Info. To read
the manual, type C-h i. The Info system comes with its own tutorial,
accessible simply by pressing h while reading the manual. Finally,
Emacs provides quite a few ways to get help, all bound to key combos
starting with C-h. Typing C-h ? brings up a complete list. Two of the
most useful, besides the tutorial, are C-h k, which lets us type any
key combo and tells us what command it invokes, and C-h w, which lets
us enter the name of a command and tells us what key combination
invokes it.

向导里还描述了其他有用的命令以及启动它们的组合键。Emacs
还提供了大量在线文档，可以通过其内置的超文本浏览器 Info
来访问。阅读这些手册只需输入 `C-h i`。这个 Info
系统也有其自身的向导，可以简单地在阅读手册时按下 `h` 来访问。
最后，Emacs 提供了好几种获取帮助信息的方式，全部绑定到了以 `C-h`
开头的组合键上。输入 `C-h ?`
就可以得到一个完整的列表。除了向导以外，还有两个最有用的帮助命令一个是
`C-h k`，它可以告诉我们输入的任何组合键所对应的命令是什么；另一个是
`C-h w`，它可以告诉我们输入的命令名字所对应的组合键是什么。

The other crucial bit of Emacs terminology, for folks who refuse to
work through the tutorial, is the notion of a buffer. While working in
Emacs, each file you edit will be represented by a different buffer,
only one of which is "current" at any given time. The current buffer
receives all input--whatever you type and any commands you
invoke. Buffers are also used to represent interactions with programs
such as Common Lisp. Thus, one common action you'll take is to "switch
buffers," which means to make a different buffer the current buffer so
you can edit a particular file or interact with a particular
program. The command switch-to-buffer, bound to the key combination
C-x b, prompts for the name of a buffer in the area at the bottom of
the Emacs frame. When entering a buffer name, hitting Tab will
complete the name based on the characters typed so far or will show a
list of possible completions. The prompt also suggests a default
buffer, which you can accept just by hitting Return. You can also
switch buffers by selecting a buffer from the Buffers menu.

对于那些拒绝看向导的人来说，有个至关重要的 Emacs
术语不得不提，那就是缓冲区（buffer）。当使用 Emacs
时，你所编辑的每个文件都将被表示成不同的缓冲区，在任一时刻只有一个缓冲区是
“当前使用的”。当前缓冲区会接收所有的输入——无论是在打字还是调用任何命令。缓冲区也用来表示与 Common
Lisp 这类程序的交互。因此，你将用到的一个常见操作就是
“切换缓冲区”，就是说将一个不同的缓冲区设置为当前缓冲区，以便可以编辑某个特定的文件或者与特定的程序交互。这个命令是
`switch-to-buffer`，对应的组合键是 `C-x b`，使用时将提示在
Emacs 框架的底部输入缓冲区的名字。当输入一个缓冲区的名字时，按 Tab
键将在输入的字符基础上对其补全，或者显示一个所有可能补全方式的列表。该提示同时推荐了一个默认缓冲区，你可以直接按回车键（Return）来选择它。也可以通过在缓冲区菜单里选择一个缓冲区来切换。

In certain contexts, other key combinations may be available for
switching to certain buffers. For instance, when editing Lisp source
files, the key combo C-c C-z switches to the buffer where you interact
with Lisp.

在特定的上下文环境中，其他组合键也可用于切换到特定的缓冲区。例如，当编辑
Lisp 源文件时，组合键 `C-c C-z` 可以切换到与 Lisp 进行交互的那个缓冲区。
