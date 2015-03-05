sum関数の実装例
==============

## プログラミングスタイル
- 手続き型(procedural)プログラミング
    - 命令型(imperative)プログラミングの一種
    - 機械(計算機)に近いアプローチ
    - 状態を変化させる(＝副作用がある)手続きを実行することよって計算する
- 関数型(functional)プログラミング
    - 宣言型(declarative)プログラミングの一種
    - 数学に近いアプローチ
    - 数学的な(＝副作用がない)関数で構成された式を評価(展開)することによって計算する


----------------------------------------
Rubyを利用して、要素数$l$の配列(またはリスト)$ns$の要素$n_i$の和を求める関数sumを実装する。


## 数学的定義
数列の和(sum)

$\sum\limits_{i=0}^{l-1} n_i = n_0 + n_1 + n_2 + ... + n_{l-2} + n_{l-1}$


## 手続き型スタイル1: ループ(while)
```ruby
# while式による単純なループ
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

#### sum1 [3, 1, 2, 5, 4] の計算過程

| ループ回数 | i | ns[i] | s  |
|:--------:|:-:|:-----:|:--:|
| (開始前) | 0 |   -   |  0 |
|   1回目  | 0 |   3   |  3 |
|   2回目  | 1 |   1   |  4 |
|   3回目  | 2 |   2   |  6 |
|   4回目  | 3 |   5   | 11 |
|   5回目  | 4 |   4   | 15 |
| (終了後) | 5 |   -   | 15 |


## 手続き型スタイル2: イテレータ(for)
```ruby
# for式によるイテレーション
def sum2(ns)
  s = 0
  for n in ns
    s += n
  end
  s
end

# Array#eachメソッド(Rubyistはfor式よりもこちらを好む)
def sum3(ns)
  s = 0
  ns.each do |n|
    s += n
  end
  s
end

# sum3のブロックを{}で表現
def sum3_2(ns)
  s = 0
  ns.each { |n| s += n }
  s
end
```


## 関数型スタイル1: 再帰
```ruby
# 単純な再帰
def sum4(ns)
  if ns.empty?
    0
  else
    x, *xs = ns
    x + sum4(xs)
  end
end

# lambda(手続きオブジェクト)による末尾再帰
def sum5(ns)
  rec = lambda do |xs, s|
    if xs.empty?
      s
    else
      y, *ys = xs
      rec.(ys, y + s)
    end
  end
  rec.(ns, 0)
end

# デフォルト引数を利用した末尾再帰
def sum6(ns, s = 0)
  if ns.empty?
    s
  else
    x, *xs = ns
    sum6(xs, x + s)
  end
end
```

#### sum4 [3, 1, 2, 5, 4] の計算過程

```ruby
   sum4 [3, 1, 2, 5, 4]
-> 3 + sum4 [1, 2, 5, 4]
-> 3 + (1 + sum4 [2, 5, 4])
-> 3 + (1 + (2 + sum4 [5, 4]))
-> 3 + (1 + (2 + (5 + sum4 [4])))
-> 3 + (1 + (2 + (5 + (4 + sum4 []))))
-> 3 + (1 + (2 + (5 + (4 + 0))))
-> 3 + (1 + (2 + (5 + 4)))
-> 3 + (1 + (2 + 9))
-> 3 + (1 + 11)
-> 3 + 12
-> 15
```

#### sum5 [3, 1, 2, 5, 4] の計算過程

```ruby
   sum5 [3, 1, 2, 5, 4]
-> rec.([3, 1, 2, 5, 4], 0)
-> rec.([1, 2, 5, 4], 3 + 0)
-> rec.([2, 5, 4], 1 + 3)
-> rec.([5, 4], 2 + 4)
-> rec.([4], 5 + 6)
-> rec.([], 4 + 11)
-> 15
```

#### sum6 [3, 1, 2, 5, 4] の計算過程

```ruby
   sum6 [3, 1, 2, 5, 4]
-> sum6([3, 1, 2, 5, 4], 0)
-> sum6([1, 2, 5, 4], 3 + 0)
-> sum6([2, 5, 4], 1 + 3)
-> sum6([5, 4], 2 + 4)
-> sum6([4], 5 + 6)
-> sum6([], 4 + 11)
-> 15
```


## 関数型スタイル2: 高階関数(inject/別名reduce)
```ruby
# Enumerable#injectメソッド
def sum7(ns)
  ns.inject(0) do |x, y|
    x + y
  end
end

# sum7のブロックを{}で表現
def sum7_2(ns)
  ns.inject(0) { |x, y| x + y }
end

# ブロックの代わりにシンボルを利用
def sum8(ns)
  ns.inject(0, :+)
end
```
