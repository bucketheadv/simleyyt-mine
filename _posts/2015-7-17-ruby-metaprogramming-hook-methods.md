---
layout: post
category: ruby
title: Ruby中的钩子方法
tagline: by Sven
tags: [Ruby, 元编程，钩子方法]
---

从事Ruby开发的工作有一段时间了，于是慢慢接触了Ruby元编程的各种奇技淫巧。第一章就先记录一下钩子方法吧。

<!--more-->

最近在写一些Rails的东西，开发过程中经常会遇到一个问题就是引用别的Gem包，而有的Gem包用起来不是特别爽，于是就想要自己定制开发一个Gem包，而在开发过程中才发现，不理解元编程真是有种寸步难行的感觉，于是对元编程的研究就开始了。首先还是说说钩子方法。

##什么是钩子方法

钩子方法是一种在程序运行时扩展程序的行为。可以理解为它是一个条件触发器，当满足这个条件时，执行对应的回调方法。Ruby中常见的钩子方法有以下几个：

###1、included

###2、extended

###3、prepended

###4、inherited

###5、method_missing

##included

这个方法用在module中定义类宏真是再好不过了，类宏是指类似`attr_accessor`这种直接在类作用域中使用的方法，在module中定义的方法被类所include进来时，该方法会成为类的实例方法，因此它只能在对象实例中调用，如果要在类中定义一个类宏，则需要使用included方法。代码如下:

{% highlight ruby %}
module A
  def self.included(base)
    base.class_eval {
      def self.a 
        puts 'a'
      end
    }
  end
end

class B
  include A
  a
end
{% endhighlight %}

在B这个类中，直接使用a方法，这个方法是在A模块中定义的，当模块A被include时会触发钩子方法included，它传入的参数为include这个module的类，即B。class_eval方法打开这个类，为它添加方法，由`def self.a`定义为include这个类的类方法而不是实例方法，由此一来，类B添加了一个类方法a，于是在类B的作用域中直接调用a方法就不成问题了。这是一个很实用的技巧，当初在编写第一个Gem时，在这个地方卡了好久，最后不得已用ActiveSupport::Concern解决的...

##extended

这个其实与include类似，只不过include包含进来的方法会被当作实例方法，而extend进来的方法会被当作类方法。extend对应的方法为extended，示例如下:

{% highlight ruby %}
module A
  def self.extended(base)
    puts "#{base} extend #{self}"
  end

  def a
    puts "a"
  end
end
class B
  extend A
  a
end
{% endhighlight %}

在A中定义的方法a被extend到B中时，可以直接调用a，因此它成为B类的一个类方法。而extended方法不用多说，其实和included方法的完全一致。

```未完待续```
