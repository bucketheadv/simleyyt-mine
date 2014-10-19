---
layout: post
category: erlang
title: CentOS yum安装erlang
tagline: by Sven
tags: [Erlang,CentOS,yum]
---

先前不知道如何在centos下使用yum安装erlang,傻呵呵地编译源码,结果遇到各种包不全的问题。

<!--more-->

##添加Erlang Solutions key支持
{% highlight c %}
rpm --import http://binaries.erlang-solutions.com/debian/erlang_solutions.asc
{% endhighlight %}

##将erlang的repo文件添加到/etc/yum.repos.d/下
{% highlight c %}
wget http://binaries.erlang-solutions.com/rpm/centos/erlang_solutions.repo
mv erlang_solutions.repo /etc/yum.repos.d/
{% endhighlight %}

##添加RPMforge支持(64位)
{% highlight c %}
wget http://packages.sw.be/rpmforge-release/rpmforge-release-0.5.2-2.el6.rf.x86_64.rpm
{% endhighlight %}

##导入key
{% highlight c %}
rpm --import http://apt.sw.be/RPM-GPG-KEY.dag.txt
{% endhighlight %}

##安装RPMforge
{% highlight c %}
rpm -i rpmforge-release-0.5.2-2.el6.rf.*.rpm
{% endhighlight %}

##更新并安装
{% highlight c %}
yum update
yum install esl-erlang
{% endhighlight %}

###`安装到此完成了！下面就可以编码确认一下了！`
网上问了一下大神，他给了一个更简单的方案：
{% highlight c %}
yum epel*
yum install erlang
{% endhighlight %}
这种方法其实是用以下方法:
{% highlight c %}
rpm -ivh http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
rpm --import /etc/pki/rpm-gpg/RPM-GPG-KEY-EPEL-6
yum install yum-priorities
yum install erlang
{% endhighlight %}
此时确实可以用`yum install erlang`了,这种方式安装的包其实更全,最好使用这种方式安装。
