---
layout: post
category: erlang
title: Erlang的编译和运行
tagline: by Sven
tags: [Erlang,Makefile]
---

这次主要记录一下Erlang运行的几个方式以及makefile的编写。

<!--more-->

##设置代码的搜索路径
Erlang的运行时系统使用一种代码自动载入机制。可以通过`code:get_path()`来查看当前载入的路径值。
下面是两个最常用的用来操作载入路径的函数。

####-spec code:add_patha(Dir) => true | {error,bad_directory}
向载入路径开头添加一个新的目录

####-spec code:add_pathz(Dir) => true | {error,bad_directory}
向载入路径末端添加一个新的目录

这两个函数会返回不同的结果，如果出现了载入错误模块时，可以调用`code:all_loaded()`来查看加载模块的总列表，
也可以通过`code:clash()`来查看哪里出了错。

##通过命令来启动Erlang
{% highlight erlang %}
erl -pa Dir1 -pa Dir2 ... -pz DirK1 -pz DirK2
{% endhighlight %}
这个命令的作用和`add_patha`、`add_pathz`作用一样，只是这里是通过shell命令来启动而已。

## 运行程序的不同方式

### 直接使用`erlc xxx.erl`来编译
使用这种方式编译的最大问题是当文件数量很大时，编译的效率就显得过于低下了。

### 在Erlang shell里使用`c(xxx)`来编译，xxx为模块名称。
这种方式一般在调试代码时使用，并且它是支持热更的一种重要方式。

### 快速脚本编程 
使用-eval参数可以进行快速脚本编程，例如:
{% highlight erlang %}
erl -eval 'io:format("Memory: ~p~n",[erlang:memory(total)]).' -noshell -s init stop
{% endhighlight %}

### 在shell里编译和运行
{% highlight erlang %}
erlc hello.erl
erl -noshell -s hello start -s init stop
{% endhighlight %}
这段命令的作用是首先编译hello.erl,再运行`hello:start()`和`init:stop()`两个函数。`noshell`表示不开启erlang终端。

### 作为Escript运行
创建一个名为`hello`文件，在文件中输入以下代码：

{% highlight erlang %}
#!/usr/bin/env escript
main(Args) ->
	io:format("Hello world~n").
{% endhighlight %}
保存后返回终端，然后输入以下命令：
{% highlight erlang %}
$ chmod u+x hello
$ ./hello
Hello world
{% endhighlight %}
这种情况下作为脚本解释不用编译就能直接运行，但需要注意的是要给文件添加执行权限。

### 运行带命令行参数的程序
新建一个名为fac.erl的文件，并输入以下代码:
{% highlight erlang %}
-module(fac).
-export([main/1]).

main([A]) ->
	I = list_to_integer(atom_to_list(A)),
	F = fac(I),
	io:format("factorial ~w = ~w~n",[I,F]),
	init:stop().

fac(0) -> 1;
fac(N) -> N * fac(N - 1).
{% endhighlight %}
然后编译并运行。
{% highlight erlang %}
$ erl fac.erl
$ erl -noshell -s fac main 5
factorial 5 = 120
{% endhighlight %}
也可以将以上代码修改代码头后写成Escript脚本文件，写成脚本文件fac，运行如下:
{% highlight erlang %}
$ ./fac 5
factorial 5 = 120
{% endhighlight %}

### 重头戏，Makefile!
如果要编译的文件很多，编写Makefile就很有必要了。一个精简的Makefile模板如下：
{% highlight c %}
## 以下几行不需要改动
.SUFFIXES: .erl .beam 

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

## 这里写入要编译的模块
MODS = module1 module2 ... moduleN 

## 这里的'.'表示当前文件夹，这行的作用是运行当前文件夹下module1模块中的start函数
## 如果不需要运行程序，此句当省略
all:compile
	${ERL} -pa '.' -s module1 start

## 这里的subdirs可以省略，否则与下文的subdirs对应
compile: ${MODS: %=%.beam} subdirs

## 如果有子文件夹，进入该子文件夹后再进行Makefile
## 此时子文件夹中也需要有Makefile文件
subdirs:
	cd dir1;$(MAKE)
	cd dir2;$(MAKE)

## 移除所有代码
## 如果子文件夹有Makefile,则也需要在子文件夹中make clean
clean:
	rm -rf *.beam erl_crash.dump
	cd dir1; $(MAKE) clean

{% endhighlight %}
详细的编写方法，都写在了注释里，无需再多花时间出来解释。

## 如果你的Erlang崩溃了

### 阅读转储文件
如果erlang崩溃了，会留下一个erl_crash.dump文件，此时可以启动一个Web故障分析器，
输入以下命令：
{% highlight erlang %}
1> crashdump_viewer:start().
WebTool is available at http://localhost:8888/
Or http://127.0.0.1:8888/
ok
{% endhighlight %}
然后打开浏览器输入[http://localhost:8888](http://localhost:8888)，这样就能查看错误日志了。
