---
layout: post
author: sven
tagline: by Sven
tags: [git,github]
categories: git
title: Git那些事儿
---

##使用Github提交代码
作为一名程序员，与git打交道是常有的事儿了，虽然公司里一直在使用svn，但感觉
跟git比还是差了那么些，至于好在哪儿就不多提了，各人有各人喜好，适合自己
的才是是好的。最近由于使用svn较多，也很少有时间来做自己的项目，导致许多git
的使用方法越来越陌生，特在此记录一些常用的操作，具体的git使用方法还是得
去查看官方文档。

个人还是喜欢git提交到github的方式来管理，gitolite什么的也实在没时间去折腾，
要使用git + github，首先还是安装git的客户端，windows下直接下载exe安装，
[点击此处](http://git-scm.com/download/ "git客户端下载")进行下载。linux用户
使用apt-get install git或yum install git或其他(根据实际情况)命令进行安装。

###git初始化配置和首次提交
{% highlight c %}
1> git config --global user.name "Your Name"
2> git config --global user.email you@yourdomain.com #然后进入你的工作目录
3> git init
4> git add .  # git add file,file表示你要提交的文件名, .表示提交当前目录下所有文件和文件夹。
5> git commit -m "first commit" # 括号内一般填写本次提交的内容概述
6> git remote add origin https://github.com/yourgitaccount/yourreposity.git # yourgitaccount为你的github帐号,yourreposity为你在github创建的仓库名
7> git push -u origin master # 将提交的修改上传到github
{% endhighlight %}
值得注意的是，现在github在push时可能会出现

	error: The requested URL returned error: 403 while accessing https://github.com/amonest/python-scripts.git/info/refs
	fatal: HTTP request failed

这样的错误，此时需要执行以下命令：

	git remote set-url origin https://yougithubaccount@github.com/yourgithubaccount/yourreposity.git

其中yourgithubaccount和yourreposity分别为你的github帐号和仓库名，执行完毕后，顺利push。

每次修改或添加新文件需要提交时，都需要执行一次命令4~5，尤其是5经常容易
被忽略，命令6~7仅需上传到github上时需要执行。

###本地操作的基本命令

####git init
除了`git init`之外，还可以使用`git init-db`来创建一个空的git库。一般来
说，当你第一次提交一个项目时才需要在项目的根目录下执行此命令，执行后会
产生一个`.git`的子目录，所有文件变化信息都在保存在此目录下。其中`.git`
目录下有一个config文件，可以修改其中的配置。

####git add
将当前目录下的更改或者新增文件添加到git索引中，提交前必须先执行它。
`git add dir`表示添加一个目录，包括其下的所有子目录和文件,`git add f1 f2`
添加f1、f2两个文件到索引中，`git add .`添加当前目录下所有文件和文件夹。

####git rm
从当前工作目录中和索引中删除文件。如果参数是一个目录，则会递归删除。
`git rm -r *`删除该目录下所有,`git rm f1`,删除文件f1,包括本地文件和索引
文件，`git rm --cached f1`，只从索引中删除f1。

####git commit
`git commit -m "notes"`,notes表示的是注释。也可以使用`-a`参数来强行提交
，用于将没有通过git add标识的变化一并提交，通常不使用这个参数。

####git checkout
`git checkout -b name`，用于创建分支，name表示分支名。分支上的工作完成
后使用`git checkout master`回到主分支，在确定分支上的工作无误后可以通
过`git merge master name`将分支名为name的分支合并到主分支master上。

####git branch
使用`git branch -D name`删除分支名为name的分支。使用`git branch name`
创建分支，但此时不会切换分支，要想在创建分支时切换，使用`git checkout -b name`方式创建分支。
`git branch`可以列出本地git库中所有分支，*表示当前分支。`git branch -r`
列出服务器git库所有分支。

####git mv
`git mv a.erl b.erl`将a.erl重命名为b.erl。

####git diff
将本地代码和索引代码作比较，或将索引代码和仓库代码作比较。
`git diff`比较工作目录和索引中的代码，`git diff --cached`比较索引和本地仓库中的代码。

####git config
用于新增和修改git的设置，比如先前的设置user.name和user.email。

####git tag
关于标签的操作，本人也不是很熟悉。

####git clone
`git clone https://github.com/account/reposity.git`从github上下载远程的
git库。

####git push & git pull
`git push origin master`将本地origin分支提交到远程库上的master分支。
git push是直接覆盖，貌似不会合并文件，也就是说不会有覆盖提示。
`git pull`则是将远程库更新到本地库中。

####git fetch
从远程库中下载代码，不会自动merge，比git pull安全。
