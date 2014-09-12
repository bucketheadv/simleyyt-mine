---
layout: post
category: erlang
title: Erlang的并发编程
tagline: by Sven
tags: [Erlang,Concurrent]
---

最近工作比较忙，以致没有太多时间自由分配来学习，好在Erlang的进度并没有落下。

<!--more-->

##使用spawn分裂进程 
Erlang是一种并发型的语言，在Erlang里可以通过spawn来创建进程，每个进程都是一个
独立运行的虚拟机，进程之间通过'消息'来进行通信，发送消息的格式为` Pid ! Message`,
表示向进程ID为Pid的进程发送一个消息Message。
例如：
{% highlight erlang linenos %}
-module(demo).
-export([start/0,area/2]).
start() -> spawn(?MODULE,loop,[]).
area(Pid,Request) ->
	rpc(Pid,Request).
rpc(Pid,Request) ->
	Pid ! {self(),Request},
	receive
		Response ->
			Response
	end.
loop() ->
	receive
		{From,{rectangle,Width,Ht} ->
			From ! Width * Ht,
			loop();
		{From,{circle,R}} ->
			From ! 3.14159 * R * R,
			loop();
		{From,Other} ->
			From ! {error,Other},
			loop()
	end.
{% endhighlight %}
其中?MODULE是一个宏，表示当前模块名，即当前文件名(不含文件拓展名，此处为demo)，loop为要创建进程的函数，[]为传入参数。
也可以通过`spawn(fun demo:loop/0).`的方式来创建一个进程，返回值为进程ID，一般称为Pid。
这段代码的作用是，开启一个进程用于接收计算面积值，当接收到的消息匹配rectangle或circle时，计算结果并传递给返回给发送进程，否则返回给发送进程错误信息。
使用如下方式进行测试：
{% highlight erlang %}
1> Pid = demo:start().
2> demo:area(Pid,{rectangle,10,9}).
3> demo:area(Pid.{circle,10}).
4> demo:area(Pid,{square,10}).
{% endhighlight %}
第1行代码会返回一个进程的Pid，第2行会计算出矩形面积并返回给发送进程，第3行会计算出圆形面积返回给发送进程，第4行会报异常。
这是由于在loop函数中没有定义与square相匹配的处理方式，因此会返回发送进程error信息，error信息的内容为`{error,{square,10}}`。

##上例代码的改进
由于loop函数中并没有发送给发送进程(客户端)自己的Pid,因此客户端会接收所有的消息，当代码趋于复杂时会不利于调试，因此
我们可以尝试让服务器将自己的Pid发送给客户端，以区别是否是服务器返回的消息异或是其他客户端进程发送的消息，修改后的loop函数如下：
{% highlight erlang linenos %}
loop() ->
	receive
		{From,{rectangle,Width,Ht}} ->
			From ! {self(),Width * Ht },
			loop();
		{From,{circle,R}} ->
			From ! {self(),3.14159 * R * R},
			loop();
		{From,Other} ->
			From ! {self(),{error,Other}},
			loop()
	end.
{% endhighlight %}
此处引入了self()函数，它将返回当前进程的Pid，如此一来，客户端所接收到的信息中就会包含服务器的进程ID，我们也就知道接收到的消息
来自于哪个进程了。

##进程接收消息超时
有时我们会遇到一种情况，客户端向服务器发送了一条请求，并等待接收服务器返回的消息，
但由于服务器运行出现故障或网络中断导致客户端长时间不能接收到服务器返回的消息
从而使客户端进入无限的等待。为了避免这种情况，我们可以将客户端代码加入超时处理的代码。
例如:
{% highlight erlang linenos %}
-module(timeout).
-export([flush_buffer/0,sleep/1]).
flush_buffer() ->
	receive
		_Any ->
			flush_buffer()
	after 0 ->
		true
	end.
sleep(T) ->
	receive
	after T ->
		true
	end.
{% endhighlight %}
在这段代码中，使用after关键字来定义一个超时，后面接一个整数代表超时的毫秒数，
一旦超时，代码将终止执行，也意味着进程结束，此处是一个超时为0的超时，超时为0的语句
会立即触发一个超时，但在此之前，系统会对所有消息模式进行匹配。此处的flush_buffer
函数的作用就在于接收所有消息，由于没有对其进行处理（变量加下划线的意思是这个变量
的实际值我们并不关心，因为我们并不会调用它来处理任何事情），因此此函数的实际
意义在于接收所有匹配消息将等待被接收的消息列表清空（邮箱清空）。一旦没有匹配的
消息立即进入超时，超时后输出true并终止进程。如果没有设置超时，那么进程在接收完消息之后
会进入等待，因为此函数是一个尾递归。同样的sleep函数就是一个纯粹的延时函数。

##使用MFA启动进程
在上述代码中我们通过`spawn(?MODULE,loop,[])`来创建的进程，我们也提到了`spawn(fun demo:loop/0)`的方式同样也能
创建一个进程，想要确保代码可以很好的动态更新，则需要选择更好的spawn。
第1种创建方式的标准形式为`spawn(Mod,FuncName,Args)`,Args是一个参数列表，形如
[Arg1,Arg2,...,ArgN]。新进程会从函数Mod:FuncName(Arg1,Arg2,...,ArgN)开始执行。这种显示
指定模块、函数名参数列表的方式称作MFA方式。这种方式对于软件升级来说是一种正确的方法。
它可以确保代码在编译后，处于运行状态，仍然可以用新版本代码进行升级。而不采用MFA方式命名的情况，
则无法获得动态代码更新的特性。
