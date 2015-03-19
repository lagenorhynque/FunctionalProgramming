sum関数の実装例
==============

## ■ プログラミングスタイル
- 手続き型(procedural)プログラミング
    - 命令型(imperative)プログラミングの一種
    - 機械(計算機)に近いアプローチ
    - 状態を変化させる(＝副作用がある)手続きを実行することよって計算する
- 関数型(functional)プログラミング
    - 宣言型(declarative)プログラミングの一種
    - 数学に近いアプローチ
    - 数学的な(＝副作用がない)関数で構成された式を評価(展開)することによって計算する


----------------------------------------
Rubyを利用して、要素数$l$の配列(またはリスト)$ns$の要素の和を求める関数sumを実装する。


## ■ 数学的定義
#### 数列の総和(sum)

数列$\langle a_n \rangle$の第$i$項までの和を$s_i$とすると

> $s_0 = a_0$

> $s_i = s_{i-1} + a_i$

総和の記号$\sum$で表すと

> $\sum\limits_{i=0}^{l-1} a_i = s_{l-1} = a_0 + a_1 + a_2 + ... + a_{l-2} + a_{l-1}$

> $\sum\emptyset = 0$


## ■ 手続き型スタイル1: ループ(while)
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


## ■ 手続き型スタイル2: イテレータ(for)
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


## ■ 関数型スタイル1: 再帰
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
= 3 + sum4 [1, 2, 5, 4]
= 3 + (1 + sum4 [2, 5, 4])
= 3 + (1 + (2 + sum4 [5, 4]))
= 3 + (1 + (2 + (5 + sum4 [4])))
= 3 + (1 + (2 + (5 + (4 + sum4 []))))
= 3 + (1 + (2 + (5 + (4 + 0))))
= 3 + (1 + (2 + (5 + 4)))
= 3 + (1 + (2 + 9))
= 3 + (1 + 11)
= 3 + 12
= 15
```

#### sum5 [3, 1, 2, 5, 4] の計算過程

```ruby
  sum5 [3, 1, 2, 5, 4]
= rec.([3, 1, 2, 5, 4], 0)
= rec.([1, 2, 5, 4], 3 + 0)
= rec.([2, 5, 4], 1 + 3)
= rec.([5, 4], 2 + 4)
= rec.([4], 5 + 6)
= rec.([], 4 + 11)
= 15
```

#### sum6 [3, 1, 2, 5, 4] の計算過程

```ruby
  sum6 [3, 1, 2, 5, 4]
= sum6([3, 1, 2, 5, 4], 0)
= sum6([1, 2, 5, 4], 3 + 0)
= sum6([2, 5, 4], 1 + 3)
= sum6([5, 4], 2 + 4)
= sum6([4], 5 + 6)
= sum6([], 4 + 11)
= 15
```

#### 【参考】 関数型言語の場合
##### Haskell

```haskell
-- 単純な再帰
sum4 :: Num a => [a] -> a
sum4 []     = 0
sum4 (x:xs) = x + sum4 xs

-- 末尾再帰
sum5 :: Num a => [a] -> a
sum5 = rec 0
  where
    rec s []     = s
    rec s (x:xs) = rec (x + s) xs
```

##### Clojure

```clojure
;; 単純な再帰
(defn sum4 [ns]
  (if (empty? ns)
    0
    (let [[x & xs] ns]
      (+ x (sum4 xs)))))

;; 末尾再帰
(defn sum5 [ns]
  (letfn [(rec [xs s]
            (if (empty? xs)
              s
              (let [[y & ys] xs]
                (recur ys (+ y s)))))]
    (rec ns 0)))

;; アリティオーバーロードを利用した末尾再帰
(defn sum6
  ([ns]
    (sum6 ns 0))
  ([ns s]
    (if (empty? ns)
      s
      (let [[x & xs] ns]
        (recur xs (+ x s))))))
```

##### Scala

```scala
// 単純な再帰
def sum4(ns: List[Int]): Int = {
  ns match {
    case Nil => 0
    case x :: xs => x + sum4(xs)
  }
}

// 末尾再帰
def sum5(ns: List[Int]): Int = {
  @tailrec
  def rec(xs: List[Int], s: Int): Int = {
    xs match {
      case Nil => s
      case y :: ys => rec(ys, y + s)
    }
  }
  rec(ns, 0)
}

// デフォルト引数を利用した末尾再帰
@tailrec
def sum6(ns: List[Int], s: Int = 0): Int = {
  ns match {
    case Nil => s
    case x :: xs => sum6(xs, x + s)
  }
}
```


## ■ 関数型スタイル2: 高階関数(inject/別名reduce)
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

# ブロックの代わりにメソッド+のシンボルを利用
def sum8(ns)
  ns.inject(0, :+)
end
```

#### 【参考】 関数型言語の場合
##### Haskell

```haskell
-- 高階関数foldl'
sum7 :: Num a => [a] -> a
sum7 = foldl' (\x y -> x + y) 0

-- ラムダ式の代わりに演算子+を関数化して利用
sum8 :: Num a => [a] -> a
sum8 = foldl' (+) 0
```

##### Clojure

```clojure
;; 高階関数reduce
(defn sum7 [ns]
  (reduce (fn [x y] (+ x y)) 0 ns))

;; ラムダ式のリーダマクロを利用
(defn sum7_2 [ns]
  (reduce #(+ %1 %2) 0 ns))

;; ラムダ式の代わりに関数+を利用
(defn sum8 [ns]
  (reduce + 0 ns))
```

##### Scala

```scala
// 高階関数foldLeft
def sum7(ns: List[Int]): Int = {
  ns.foldLeft(0)((x, y) => x + y)
}

// ラムダ式の代わりにメソッド+とプレースホルダ構文を利用
def sum8(ns: List[Int]): Int = {
  ns.foldLeft(0)(_ + _)
}
```
