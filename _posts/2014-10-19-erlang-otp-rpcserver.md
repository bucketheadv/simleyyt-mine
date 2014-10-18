---
layout: post
category: erlang
title: 基于Erlang/OTP的rpc服务器
tagline: by Sven
tags: [Erlang,OTP,rpc]
---

最近在学Erlang/OTP,记录一下各种服务的开发过程。

<!--more-->

##首先贴上代码

###tr_server.erl
{% highlight erlang %}
-module(tr_server).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([start_link/0,start_link/1,init/1,handle_call/3,handle_info/2,handle_cast/2,code_change/3,terminate/2]).
-export([get_count/0,stop/0]).

-define(SERVER,?MODULE).
-define(DEFAULT_PORT,1055).
-record(state,{port,lsock,request_count = 0}).

start_test() ->
    {ok,_} = tr_server:start_link(?DEFAULT_PORT).

start_link(Port) ->
    gen_server:start_link({local,?MODULE},?MODULE,[Port],[]).

start_link() ->
    start_link(?DEFAULT_PORT).

init([Port]) ->
    {ok,LSock} = gen_tcp:listen(Port,[{active,true}]),
    {ok,#state{port = Port,lsock = LSock},0}.

get_count() ->
    gen_server:call(?SERVER,get_count).

stop() ->
    gen_server:cast(?SERVER,stop).

handle_call(get_count,_From,State) ->
    {reply,{ok,State#state.request_count,State}}.

handle_cast(stop,State) ->
    {stop,normal,State}.

handle_info({tcp,Socket,RawData},State) ->
    do_rpc(Socket,RawData),
    RequestCount = State#state.request_count,
    {noreply,State#state{request_count = RequestCount + 1}};
handle_info(timeout,#state{lsock = LSock} = State) ->
    {ok,_Sock} = gen_tcp:accept(LSock),
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

do_rpc(Socket,RawData) ->
    try
        {M,F,A} = split_out_mfa(RawData),
        Result = apply(M,F,A),
        gen_tcp:send(Socket,io_lib:fwrite("~p~n",[Result]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket,io_lib:fwrite("~p~n",[Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData,"\r\n$","",[{return,list}]),
    {match,[M,F,A]} =
        re:run(MFA,
               "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
               [{capture,[1,2,3],list},ungreedy]),
    {list_to_atom(M),list_to_atom(F),args_to_terms(A)}.

args_to_terms(RawArgs) ->
    {ok,Toks,_Line} = erl_scan:string("[" ++ RawArgs ++ "]. ",1),
    {ok,Args} = erl_parse:parse_term(Toks),
    Args.
{% endhighlight %}

======================================================================================

这段代码是最核心的部分，首先是实现gen_server的行为，包括
`init/1,handle_call/3,handle_info/2,handle_cast/2,terminate/2,code_change/3`这六个函数。

###init/1函数

当执行`gen_server:start/4`函数时，init/1函数会被调用，这里初始化一个socket监听网络上的tcp请求。init/1函数需要返回一个元
组， 形如{ok,State,Timeout}.State表示服务器的状态，Timeout为设置的超时时间，超时可以省略，为０时表示立即触发一个超时，即
立即处理当前邮箱里的消息。

###handle_call/3函数

这个函数只能用于处理服务器内部的消息调用，State表示服务器当前状态，与之前初始化时的State相对应，但在调用过程中可以改变
它，_From表示发送请求的客户端的进程PID，通常需要一个返回值，{reply,Reply,NewState}，NewState为服务器接下来的状态，Reply
为发送给客户端的消息,这里的第一个参数(get_count)是一个消息匹配,可以是一个原子，也可以是一个元组。也可以设置为{noreply,..}和{stop,...}，但相对不常用。noreply会让服务器继续工作，但客户端需要一个回
复消息，因此必须把回复任务委派给其他进程。用适当的参数调用stop会停止服务器。这个函数的作用是实现远程过程调用。

###handle_cast/2函数

这个函数用于实现播发，也就是没有返回值的调用（异步调用）。第一个参数(这里是stop)是一个消息匹配，State表示服务器状态。这
个函数的返回值一般为{noreply,NewState}或{stop,...}。前者改变服务器状态，后者停止服务器。

###handle_info/2函数

这个函数用于处理发送给服务器的自发性消息，即所有未经显示调用gen_server:call或gen_server:cast而到达服务器的消息，例如浏览
器访问服务器时产生的HTTP请求等，因此所有知道通用服务器PID的进程都能向它发送消息。第一个参数为一个消息匹配，第二个参数为
服务器的状态。它的返回值与handle_cast/2函数相同。

###terminate/2函数

当某个handle_开头的函数返回一个{stop,Reason,NewState}，或者服务器自身崩溃产生了一个{'EXIT',reason}时，无论它们是怎样发生
的，都会调用terminate/2函数。返回值为ok.

###code_change/3函数

你可以在服务器运行时更改它的状态。这个回调函数会在系统执行软件升级时由版本处理子系统调用。返回值为{ok,State}.

###do_rpc/2函数

第一个参数为客户端Socket，第二个参数为接收到的消息。将接收到的消息使用split_out_mfa/1函数分离出消息中的
Module,Function,Arguments存入{M,F,A},并使用apply(M,F,A)在服务器上执行这个函数，最后向客户端发送执行消息的结果（或异常）。

###split_out_mfa/1函数

先前说过，这个函数用于分离出消息中的Module,Function,Arguments信息，例如`io:format("Hello World~n")`,分离出的结果中
Module,Function,Arguments的值分别是io,format,"Hello World~n"三个值。使用re模块中的run方法提取，最后返回一个元组，其中第
一个和第二个元素是一个原子。第三个参数是一个列表。

###args_to_terms/1函数

这个函数处理参数部分，并返回一个列表。


======================================================================================

    这段代码加入了测试，添加测试需要'-include_lib("eunit/include/eunit.hrl").'，然后编写一个测试
    函数，此处为start_test/0函数。然后在Erlang Shell中使用`tr_server:test().`或
    `eunit:test(tr_server).`来进行测试。

======================================================================================


###tr_sup.erl

{% highlight erlang %}
-module(tr_sup).

-behaviour(supervisor).
-define(SERVER,?MODULE).

-export([init/1,start_link/0]).

start_link() ->
    supervisor:start_link({local,?SERVER},?MODULE,[]).

init([]) ->
    Server = {tr_server,{tr_server,start_link,[]},permanent,2000,worker,[tr_server]},
    Children = [Server],
    RestartStrategy = {one_for_one,0,1},
    {ok,{RestartStrategy,Children}}.
{% endhighlight %}

======================================================================================

标准的监控器代码，当进程tr_server崩溃时，监控器会重启一个tr_server进程。实现一个监控器只需要实现supervisor行为模式，并实
现start_link/0,init/1方法即可。

###start_link/0函数

启动一个监控器。

###init/1函数

初始化监控器，这个函数需要返回一个元组
{ok,{RestartStrategy,MaxRestarts,Time},{Tag,{Mod,Func,ArgList},Restart,Shutdown,Type,[Mod1]}}。下面来一一解释。

####RestartStrategy

这是重启策略，可选的值有one_for_one,one_for_all,simple_one_for_one,rest_for_all，
首先说one_for_one，使用这种策略时，如果一个子进程停止，则只重启该子进程。其次是one_for_all，使用这种策略时，如果一个子进
程停止，则其他子进程也停止，然后全部重启。然后是rest_for_one，使用这种策略时，如果一个子进程停止，那么启动顺序在它之后的
所有其他子进程也将停止，然后停止的这些进程重启。最后是simple_one_for_one，这种是简化的one_for_one supervisor，使用这种策
略要求它的子进程必须全是同样类型的子进程并且是动态添加的实例，例如：
{% highlight erlang %}
{simple_one_for_one,0,1}
{% endhighlight %}
，此时当supervisor启动时不会
启动任何子进程，只能通过代码动态添加子进程：
{% highlight erlang %}
supervisor:start_child(Sup,List)
{% endhighlight %}
Sup是supervisor的pid或name,List是任意一个term列表，将会动态添加到子规范的参数列表。如果启动方法指定为{M,F,A}，则子进程是
通过调用apply(M,F,A++List)来启动（）。

####MaxRestarts && Time

最大重启频率，即如果在最近的Time秒之内有超过MaxRestarts次数的重启，则supervisor停止它本身和它所有的子进程，当supervisor
停止后，下一个更高级别的supervisor（它的父监控器）进行下一步动作，重启该停止的supervisor或者终止本身，重启机制的意图是防
止一个进程由于某些原因重复性的死掉。

####Tag

用来让supervisor内部识别子规范的名字，是一个原子类型的标签，可以用它指代工作进程。

####{Mod,Func,ArgList}

指启动子进程的方法,分别是模块，函数，参数列表。该函数应该调用supervisor、gen_server、gen_event、gen_fsm其中之一的start_link
函数。

####Restart

可选的值有

* permanent

永久进程，表示子进程始终会被重启

* transient

临时进程，表示子进程决不重启

* temporary

过渡进程，表示只有在子进程异常终止时才重启，即除了normal以外的终止原因

####Shutdown

这是关闭时间。也就是工作器终止过程允许耗费的最长时间。如果超过这个时间，工作进程就会被杀掉。

可选的值有

* brutal_kill

表示子进程使用exit(Child,kill)来无条件的终止，终止之前不用做任何清理工作

* 一个整数的timeout值表示supervisor告诉子进程通过调用exit(Child,shutdown)来终止，然后等待一个exit信号返回，如果没有在指
  定时间内接收到exit信号，则子进程使用exit(Child,kill)来无条件终止
  
* 如果子进程是另一个supervisor，它应该设置为infinity来给子树足够的时间来停止

####Type

可选值为worker和supervisor，表示子进程是一个工作进程还是一个监控器

####[Mod1]

一个列表含有一个元素Mod1，
如果子进程是一个supervisor,gen_server或gen_fsm则Mod1是冋调模块的名字，
如果子进程是一个gen_event，则MODULES(即[Mod1])应该为dynamic
该信息用来在升级和降级时供release handler使用


    所有解释到此为止，最后要注意，代码中的Children列表中的子进程启动是按定义时的顺序先后启动，终止时是
    倒序终止！
    supervisor是Erlang/OTP中很重要的部分，虽然代码量很少，但是其中的参数设置却是十分讲究的，必须要好
    好掌握。
    
======================================================================================

###tr_app.erl

{% highlight erlang %}
-module(tr_app).

-behaviour(application).

-export([start/2,stop/1]).

start(_StartType,_StartArgs) ->
    case tr_sup:start_link() of 
        {ok,Pid} ->
            {ok,Pid};
        Other ->
            {error,Other}
    end.

stop(_State) ->
    ok.
{% endhighlight %}

======================================================================================

这部分最简单，就是一个简单的应用启动模块。使用`application:start(tcp_rpc).`来启动我们这个应用，它会打开一个监控器，然后
监控器打开其他的子进程。这里的tcp_rpc是下面tcp_rpc.app的文件名。要启动应用，就必须要编写相应的app文件。

======================================================================================

###tcp_rpc.app
{% highlight erlang %}
{application,tcp_rpc,
[{description,"RPC server for Erlang in OTP and actions"},
{vsn,"1.0"},
{modules,[tr_app,
          tr_sup,
          tr_server]},
{registered,[tr_sup]},
{applications,[kernel,stdlib]},
{mod,{tr_app,[]}}]
}.
{% endhighlight %}

======================================================================================

这是启动应用的配置文件，application的值为应用的名称，此处为tcp_rpc；description为应用的描述，当应用被加载时，使用
`application:loaded_applications().`可以查看该描述；vsn为版本号；modules为模块列表；registered为注册模块的列表；
applications为加载的应用程序；mod的作用尚不清楚，应该是表示启动模块。

    注意：这个应用开启后很危险，因为它可以接收远程的Erlang函数调用，这意味着当应用端口开启时，外部程序
    可以发送Erlang代码修改服务器的内容，删除服务器的文件！
    
    到此结束!

======================================================================================

