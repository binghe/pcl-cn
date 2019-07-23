# Beyond Lists: Other Uses for Cons Cells（超越列表：点对单元的其他用法）

As you saw in the previous chapter, the list data type is an illusion
created by a set of functions that manipulate cons cells. Common Lisp
also provides functions that let you treat data structures built out
of cons cells as trees, sets, and lookup tables. In this chapter I'll
give you a quick tour of some of these other data structures and the
functions for manipulating them. As with the list-manipulation
functions, many of these functions will be useful when you start
writing more complicated macros and need to manipulate Lisp code as
data.

如同你在前面章节里看到的，列表数据类型是由一组操作点对单元的函数描述的。另外，Common
Lisp
还提供了一些函数；它们可使你把点对单元构建出的数据结构看作树、集合及查询表。本章将简要介绍这其中的一些数据结构及其处理函数。和列表处理函数一样，在开始编写更复杂的宏以及需要将
Lisp 代码作为数据处理时，这其中有很多函数会很有用。
