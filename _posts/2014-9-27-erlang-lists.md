---
layout: post
category: erlang
title: Erlang列表编程
tagline: by Sven
tags: [Erlang,lists,sets]
---

无论用什么编程，数据结构都是很重要的一部分，今天就主要写一些lists的编程。

<!--more-->

##用于列表处理的BIF
经常会用到一些内置函数来进行类型转换，常用的有以下：

####atom_to_list(A)
将一个原子式L转换为一个字符列表。

####float_to_list(F)
将浮点数F转换为一个字符列表。

####integer_to_list(I)
将整数I转换为一个字符列表。

####tuple_to_list(T)
将一个元组T转换为列表。

####pid_to_list(Pid)
将一个进程ID(Pid)转换为列表。

相应的还有list_to_atom,list_to_integer,list_to_float,list_to_tuple,list_to_Pid。

####hd(L)
返回列表L的第一个元素。

####tl(L)
返回列表L的尾部。

####length(L)
返回列表L的长度。
	
	以上据说的`字符列表`都是ASCII的表现形式，即表示的是字符的ASCII值。

##常用的列表(lists)处理函数

####member(X,L)
X是否是列表L的元素，是返回true,否返回false。

####append(A,B)
连接两个列表A和B。

####reverse(L)
将列表L元素倒序。

####delete(X,L)
从列表L中删除元素X并返回一个新列表。

##常用的集合(sets)函数
在Erlang中，集合是一个不包含重复元素的无序列表。
集合的操作函数如下：

####new()
返回一个空集合。

####add_element(X,S)
返回将元素X并入集合S后所产生的新集合。

####del_element(X,S)
返回从集合S中删除元素X后产生的新集合。

####is_element(X,S)
元素X是否在集合S中，是返回true,否返回false。

####is_empty(S)
集合S是否为空，是返回true,否返回false。

####intersection(S1,S2)
返回S1和S2的交集。


##函数式参数

###map

---

函数map(Func,List)返回一个列表L，其中的元素由函数Func依次作用于List中的各个元素得到。

{% highlight erlang %}
map(Func,[H|T]) ->
	[apply(Func,[H])|map(Func,T)];
map(Func,[]) ->
	[].

> lists:map(fun(X) -> math:factorial(X) end,[1,2,3,4,5,6,7,8]).
[1,2,6,24,120,720,5040,40320]
{% endhighlight %}
这段函数调用时的作用是生成参数列表中所有元素的阶乘并返回一个新列表，这里需要注意的是apply调用，
`apply(Mod,Func,[Params])`等同于`Mod:Func(Params)`调用，尤其需要注意apply调用时参数里的`[]`符号。
还要注意，`apply({Mod,Func},[Params])`这种形式支持在控制台下使用，但貌似并不支持在文件代码中使用，
因此使用`{Mod,Func}`作为参数传递时可能会遇到意想不到的错误，最好使用`fun Mod:Func/N`的形式作为参数传递。
`lists:map(fun(X) -> math:factorial(X) end,[1,2,3,4,5,6,7,8]).`等价于`lists:map(fun math:factorial/1,[1,2,3,4,5,6,7,8]).`。

---

###filter
函数filter(Pred,List)对列表List中的元素进行过滤，仅保留令Pred的值为`true`的元素。Pred是一个返回`true`或`false`的函数。

{% highlight erlang %}
filter(Pred,[H|T]) ->
	case apply(Pred,[H]) of 
		true ->
			[H|filter(Pred,T)];
		false ->
			filter(Pred,T)
	end;
filter(_,[]) ->
	[].

> lists:filter(fun(X) -> math:even(X) end,[1,2,3,4,5,6,7,8,9,10]).
[2,4,6,8,10]
{% endhighlight %}
上例调用用于过滤并产生一个偶数列表。

##排序算法

###快速排序
下面是一段快速排序的实现代码：
{% highlight erlang %}
qsort([]) -> [];
qsort([H|T]) ->
	qsort([X || X <- T, X < H]) ++ [H] ++ qsort([X || X <- T, X >= H]).
{% endhighlight %}
这段代码开始看的时候还真是费劲，不过现在好歹是看懂了...首先是把列表头作为中值，
分别找出比列表头小的值的列表然后递归排序，加上表头，再加上比列表头大的值的列表的递归排序，
这里的++表示的是连接两个列表。需要注意`[F(X) || X <- L]`这种形式，表示的是X取值于列表L，并
求F(X)后所生成的列表。(要牢记这点，另外区别与`[H|T]`的区别，`[H|T]`中H是表头，T是表尾，|是连接符号。)

	感觉很难懂的东西，多看看，时候一到自然而然地就懂了。也不知道是不是老了，汗~^v^
