sum関数の実装例
==============

## プログラミングスタイル
- 手続き型(procedural)プログラミング、命令型(imperative)プログラミング
- 関数型(functional)プログラミング、宣言型(declarative)プログラミング


Rubyを利用して、要素数$l$の配列(またはリスト)$ns$の要素$n_i$の和を求める関数sumを実装する。

## 数学的定義
数列の和(sum)

$\sum\limits_{i=0}^{l-1} n_i = n_0 + n_1 + n_2 + ... + n_{l-2} + n_{l-1}$


## 手続き型スタイル1: ループ(while)
```ruby
# Ruby

def sum1(ns)
  s = 0
  i = 0
  while i < ns.length
    s += ns[i]
    i += 1
  end
  s
end
```


## 手続き型スタイル2: イテレータ(for)
```ruby
# Ruby

def sum2(ns)
  s = 0
  for n in ns
    s += n
  end
  s
end

def sum3(ns)
  s = 0
  ns.each do |n|
    s += n
  end
  s
end

def sum3_2(ns)
  s = 0
  ns.each { |n| s += n }
  s
end
```


## 関数型スタイル1: 再帰
```ruby
# Ruby

def sum4(ns)
  if ns.empty?
    0
  else
    x, *xs = ns
    x + sum1(xs)
  end
end

def sum5(ns)
  rec = lambda do |xs, s|
    if xs.empty?
      s
    else
      y, *ys = xs
      rec.(ys, s + y)
    end
  end
  rec.(ns, 0)
end

def sum6(ns, s = 0)
  if ns.empty?
    s
  else
    x, *xs = ns
    sum3(xs, s + x)
  end
end
```


## 関数型スタイル2: 高階関数(inject/別名reduce)
```ruby
# Ruby

def sum7(ns)
  ns.inject(0) do |x, y|
    x + y
  end
end

def sum7_2(ns)
  ns.inject(0) { |x, y| x + y }
end

def sum8(ns)
  ns.inject(0, :+)
end
```
