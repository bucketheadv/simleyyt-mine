---
layout: post
category: go
title: Go环境安装
tagline: by Sven
tags: [go,golang]
---

Erlang不打算再深入下去了,因为适用面太窄再加上没有相关经验很难再转入行，最近打算把Java复习并提高一下，然后开始主攻Go语言。

<!--more-->

##安装
直接apt-get(或yum) install golang，但可能需要先将golang库添加到apt-get(yum)源，并update。安装后的文件夹在`/usr/lib/go`(
可能不一样),但如果不在，可以执行`locate`或`find`命令查找它。

##设置gocode库
gocode库用于下载golang的相关应用，我们在执行`go get -u url`时所下载的目标文件夹，这个文件夹很重要，我将它放在~/gocode文件
夹下。打开`/etc/profile`文件，在最后加入
{% highlight lisp %}
export GOPATH=~/gocode
export PATH=$PATH:$GOPATH/bin
{% endhighlight %}
 保存文件，在shell执行`source /etc/profile`，然后我们就可以执行`go get`命令来下载相关文件了。

##配置开发环境
无论是liteide或者idea intellij，感觉还是用起来不顺手，于是着手安装emacs的go-mode。emacs默认是没有go-mode的，因此需要安装
它的依赖包。shell执行
{% highlight lisp %}
apt-get install golang-mode gocode-auto-complete-el
go get -u github.com/nsf/gocode
cp $GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el ~/.emacs.d/
{% endhighlight %}
打开~/.emacs文件加入以下几行
{% highlight lisp %}
(require 'go-mode-load)
(require 'go-autocomplete)
(require 'auto-complete-config)
{% endhighlight %}
**需要注意的是，需要auto-complete插件的支持，即是需要安装它，由于该插件先前已安装，所以不在此处阐释它。**

    到此为止，环境配置算是完成了，使用emacs编辑go文件可以看到它已经可以为我们做到补全代码的功能了。
