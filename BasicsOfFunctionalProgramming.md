関数型プログラミングの基本
====================

繰り返し処理の実現方法という観点から、関数型プログラミングの基本的な概念を紹介する。

題材として関数型言語の入門でよく取り上げられる「フィボナッチ数」([Fibonacci number])の計算を用いる。

$i$ 番目のフィボナッチ数 $F_i$ は、以下のように定義される。

> $F_0 = 0$

> $F_1 = 1$

> $F_i = F_{i-2} + F_{i-1}, i \ge 2$

$0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...$ と続き、直前の2項の和が次の項になっている。

[Fibonacci number]: http://en.wikipedia.org/wiki/Fibonacci_number


## 0. ループ
**手続き型([procedural])**言語では、一般的に繰り返しをループによって表現する。

```python
# Python
def fibonacci(i):
    a, b = 0, 1
    for n in range(i):
        a, b = b, a + b
    return a
```

上記Pythonのコードでは、a = 0, b = 1としてそれぞれ初期化し、aにbを、bにaとbの和を代入する処理をi回繰り返している。
ここでは、変数への値の代入により状態を変化させていることから、**副作用([side effect])**があると表現される。

このような変数に代入を行うことで処理を実行していくスタイルは機械寄りの(低レベルな)アプローチであり、一行一行命令を記述していくものであることから**命令型([imperative])**と呼ばれる。

変数やループ構造が目立ち、数学的な定義(プログラムの仕様)との関係が分かりづらくなっている。
また、フィボナッチ数の計算は非常にシンプルだが、登場する変数が多くなったり、処理が複雑になったりすると、状態の変化をたどるのが困難になる可能性がある。

[procedural]: http://en.wikipedia.org/wiki/Procedural_programming
[side effect]: http://en.wikipedia.org/wiki/Side_effect_(computer_science)
[imperative]: http://en.wikipedia.org/wiki/Imperative_programming


## 1. 再帰
**関数型([functional])**言語では、手続き型言語で一般的なループの代わりに**再帰([recursion])**によって繰り返しを表現することが多い。

```haskell
-- Haskell
fibonacci1 :: Int -> Integer
fibonacci1 0 = 0
fibonacci1 1 = 1
fibonacci1 i = fibonacci1 (i - 2) + fibonacci1 (i - 1)
```
```clojure
;; Clojure
(defn fibonacci1 [i]
  (cond
    (= i 0) 0N
    (= i 1) 1N
    :else   (+ (fibonacci1 (- i 2)) (fibonacci1 (- i 1)))))
```

Haskell、Clojureともに数学的な再帰的定義をほぼそのまま表現した、シンプルなコードになっている。
ちなみにHaskellでは、引数に対する**パターンマッチング([pattern matching])**を利用している。
これにより、引数の値が0の場合、1の場合、その他の場合に対応する式の値を返すという条件分岐を非常に簡潔に表現できる。

こうした数学的なアプローチは、処理すべき命令ではなく満たすべき定義や条件(仕様)を記述していくスタイルであることから、**宣言型([declarative])**と呼ばれる。

関数型言語における純粋な「関数」(function)は**参照透過性([referential transparency])**を持つ。
これは、変化する状態(可変状態)を持たず副作用がないことにより、同じ条件に対して必ず同じ結果が得られることが保証されていることを意味する。
関数型言語では可変状態/副作用のない参照透過な関数を基本要素としてプログラムが構成される。
可変状態がないため状態の変化を管理する必要がなくなり、並列/並行処理として実行するのも容易になる。

ただし、単純な再帰としてプログラムを実装した場合、関数呼出しの繰り返しによりスタックオーバーフローが発生する可能性がある。
また、フィボナッチ数の場合、1回の関数呼出しにつき2回の再帰呼出しが生じるため計算量が指数的に増大し、大きな入力値に対して計算効率が非常に悪い(→**メモ化([memoization])**で回避することもできる)。

[functional]: http://en.wikipedia.org/wiki/Functional_programming
[recursion]: http://en.wikipedia.org/wiki/Recursion_(computer_science)
[pattern matching]: http://en.wikipedia.org/wiki/Pattern_matching
[declarative]: http://en.wikipedia.org/wiki/Declarative_programming
[referential transparency]: http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)
[memoization]: https://en.wikipedia.org/wiki/Memoization


## 2. 末尾再帰
単純な再帰実装の問題を解決するため、**末尾再帰([tail recursion])**という特殊な形式の再帰に変形することがある。
末尾再帰とは、関数内部で最後に実行される処理が再帰呼出しになっている再帰のことをいう。

```haskell
-- Haskell
fibonacci2 :: Int -> Integer
fibonacci2 i = fib i 0 1
  where
    fib 0 a _ = a
    fib n a b = fib (n - 1) b (a + b)
```
```clojure
;; Clojure
(defn fibonacci2 [i]
  (letfn [(fib [n a b]
            (if (zero? n)
              a
              (recur (dec n) b (+ a b))))]
    (fib i 0N 1N)))
```

上記HaskellとClojureの例では、関数fibの第2引数a、第3引数bとしてフィボナッチ数列の2項の値を持たせることで、再帰呼出しが最後に行われることを可能にしている。

言語や処理系にもよるが、多くの関数型言語では末尾再帰関数が**末尾呼出し最適化(TCO; tail call optimization)**により構造的に命令型のループと同等の処理に変換され、処理が効率化される。
関数呼出しの繰り返しがループに変換されるため、スタックオーバーフローを防止することができる。
コードの処理内容も命令型ループによく似ている。

[tail recursion]: http://en.wikipedia.org/wiki/Tail_call


## 3. 高階関数
繰り返しを再帰で表現するのが関数型プログラミングの基本ではあるが、通常は自力で再帰関数を実装する代わりにライブラリの**高階関数([higher-order function])**を利用する。
高階関数とは、引数として関数を受け取る、または戻り値として関数を返す関数のことである。

```haskell
-- Haskell
fibonacci3 :: Int -> Integer
fibonacci3 i = fst $ foldl' fib (0, 1) [1..i]
  where
    fib (a, b) _ = (b, a + b)
```
```clojure
;; Clojure
(defn fibonacci3 [i]
  (letfn [(fib [[a b] _]
            [b (+ a b)])]
    (first (reduce fib [0N 1N] (range 0 i)))))
```

上記Haskellコードでは、畳み込み関数の一種foldl'を利用し、aをbに、bをaとbの和に置き換える処理をi回繰り返している。
Clojureでも同等の関数reduceを利用している。

明示的に再帰処理を実装するよりも、典型的な繰り返し処理は抽象化されたライブラリの高階関数に任せ、固有のロジックを持った関数の実装に集中したほうが効率良くコーディングすることができ、コードの可読性も向上する。

[higher-order function]: http://en.wikipedia.org/wiki/Higher-order_function


## 4. 遅延評価
一部の関数型言語では**遅延評価([lazy evaluation])**を利用したコードを書くことができる。
遅延評価とは、式の評価を計算で必要になるまで遅らせることをいう。
一方、大多数の言語では通常、式は**先行評価([eager evaluation])**、つまり宣言された時点で評価される。
遅延評価により、例えば無限に続く数列を定義した上で、必要な部分だけ利用するというようなことが簡単に記述できる。

```haskell
-- Haskell
fibonacci4 :: Int -> Integer
fibonacci4 i = fibs !! i
  where
    fibs = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)
```
```clojure
;; Clojure
(defn fibonacci4 [i]
  (let [fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N]))]
    (nth fibs i)))
```

Haskell、Clojureともに、関数iterateを利用して
(0, 1), (1, 1), (1, 2), (2, 3), (3, 5), (5, 8), (8, 13), (13, 21), (21, 34), (34, 55), ...
と続いていく無限リストを定義し、i番目の要素の1つ目の値を取り出している。

Haskellでは遅延評価がデフォルトの評価戦略となっている。
一方、Clojureは先行評価が基本だが、遅延評価されるシーケンス(遅延シーケンス)を利用することができる。

1つのフィボナッチ数列の各項にもう1つのフィボナッチ数列を1項ずつずらして足し合わせた数列は再びフィボナッチ数列になることを利用して、Haskellでは以下のように実装することもできる。

```haskell
-- Haskell
fibonacci5 :: Int -> Integer
fibonacci5 i = fibs !! i
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

また、Clojureでは以下のようにlazy-seqマクロにより遅延シーケンスとしてフィボナッチ数列を生成することもできる。

```clojure
;; Clojure
(defn fibonacci5 [i]
  (letfn [(fibs [a b] (cons a (lazy-seq (fibs b (+ a b)))))]
    (nth (fibs 0N 1N) i)))
```

特に巨大なデータ構造や無限に続くデータ構造を扱う場合に、遅延評価が利用できるとシンプルな定義と効率を両立させることができる。

[lazy evaluation]: http://en.wikipedia.org/wiki/Lazy_evaluation
[eager evaluation]: http://en.wikipedia.org/wiki/Eager_evaluation


## 別解: 行列の利用

フィボナッチ数列を数学的に行列(matrix)として表現すると、以下のようになる。
> $\begin{pmatrix} F_{i+1} & F_i \\\\ F_i & F_{i-1} \end{pmatrix} = {\begin{pmatrix} 1 & 1 \\\\ 1 & 0 \end{pmatrix}}^i$

したがって、 $\begin{pmatrix} 1 & 1 \\\\ 1 & 0 \end{pmatrix}$ の $i$ 乗を計算することで、 $F_i$ の値を求めることができる。

2×2の行列の積は以下のように定義されている。
> $\begin{pmatrix} a_{11} & a_{12} \\\\ a_{21} & a_{22} \end{pmatrix}\begin{pmatrix} b_{11} & b_{12} \\\\ b_{21} & b_{22} \end{pmatrix} = {\begin{pmatrix} a_{11}b_{11} + a_{12}b_{21} & a_{11}b_{12} + a_{12}b_{22} \\\\ a_{21}b_{11} + a_{22}b_{21} & a_{21}b_{12} + a_{22}b_{22} \end{pmatrix}}$

また、2×2の行列の単位行列(単位元)は
> $\begin{pmatrix} 1 & 0 \\\\ 0 & 1 \end{pmatrix}$

である。

以上のことから、行列 $\begin{pmatrix} a & b \\\\ c & d \end{pmatrix}$ をリスト[a, b, c, d]で表すとすると、以下のようにフィボナッチ数を計算することができる。

```haskell
-- Haskell
fibonacci6 :: Int -> Integer
fibonacci6 i = iterate (prod [1, 1, 1, 0]) [1, 0, 0, 1] !! i !! 1
  where
    prod [a11, a12, a21, a22] [b11, b12, b21, b22] =
        [ a11 * b11 + a12 * b21, a11 * b12 + a12 * b22
        , a21 * b11 + a22 * b21, a21 * b12 + a22 * b22
        ]
```
```clojure
;; Clojure
(defn fibonacci6 [i]
  (letfn [(prod [[a11 a12 a21 a22] [b11 b12 b21 b22]]
            [(+ (* a11 b11) (* a12 b21)) (+ (* a11 b12) (* a12 b22))
             (+ (* a21 b11) (* a22 b21)) (+ (* a21 b12) (* a22 b22))])]
    (nth (nth (iterate #(prod [1N 1N 1N 0N] %) [1N 0N 0N 1N]) i) 1)))
```

上記Haskellのコードでは、関数prodを**部分適用([partial application])**している。
部分適用とは、複数の引数を受け取る関数に一部の引数のみ渡すことをいい、これにより関数どうしを柔軟に組み合わせることが可能になる。

ここで、指数法則
> $a^{mn} = {(a^m)}^n$

により、指数が偶数の場合には $a^{2n} = {(a^2)}^n$ と変形できることを利用すると、 ${\begin{pmatrix} 1 & 1 \\\\ 1 & 0 \end{pmatrix}}^i$ の計算を大幅に効率化することができる。

```haskell
-- Haskell
fibonacci6' :: Int -> Integer
fibonacci6' i = pow prod [1, 1, 1, 0] i [1, 0, 0, 1] !! 1
  where
    pow f x n a
        | n == 0    = a
        | even n    = pow f (f x x) (n `div` 2) a
        | otherwise = pow f x (n - 1) (f x a)
    prod [a11, a12, a21, a22] [b11, b12, b21, b22] =
        [ a11 * b11 + a12 * b21, a11 * b12 + a12 * b22
        , a21 * b11 + a22 * b21, a21 * b12 + a22 * b22
        ]
```
```clojure
;; Clojure
(defn fibonacci6' [i]
  (letfn [(pow [f x n a]
            (cond
              (zero? n) a
              (even? n) (recur f (f x x) (quot n 2) a)
              :else     (recur f x (dec n) (f x a))))
          (prod [[a11 a12 a21 a22] [b11 b12 b21 b22]]
            [(+ (* a11 b11) (* a12 b21)) (+ (* a11 b12) (* a12 b22))
             (+ (* a21 b11) (* a22 b21)) (+ (* a21 b12) (* a22 b22))])]
    (nth (pow prod [1N 1N 1N 0N] i [1N 0N 0N 1N]) 1)))
```

上記のHaskellコードでは、関数powでの条件分岐を**ガード([guard])**で表現している。
「| 述語 = 式」という形式を連ねることで、上から順に見て初めて述語を満たす行の式の値を返すことができる。
特にHaskellでは、パターンマッチングとガードを効果的に組み合わせることで複雑な条件分岐をとても短く分かりやすく記述することができる。

[partial application]: http://en.wikipedia.org/wiki/Partial_application
[guard]: http://en.wikipedia.org/wiki/Guard_(computer_science)
