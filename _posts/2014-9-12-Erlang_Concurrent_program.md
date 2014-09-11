---
layout: post
category: erlang
title: Erlang的并发编程
tagline: by Sven
tags: [Erlang,Concurrent]
---

#使用spawn分裂进程 #
例如：
{% highlight erlang %}
-module(demo).
-export([start/0]).
start() -> spawn(?MODULE,loop,[]).
loop() ->
	receive
	end.
{% endhighlight %}
其中?MODULE是一个宏，表示当前模块名，即当前文件名(不含文件拓展名)。

