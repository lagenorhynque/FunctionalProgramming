product関数の実装例
==================

## ■ プログラミング言語
- 手続き型(procedural)言語
- オブジェクト指向(object-oriented)言語
- 関数型(functional)言語
- 論理型(logic)言語


----------------------------------------
様々な言語で、要素数$l$の配列(またはリスト)$ns$の要素の積を求める関数productを実装する。


## ■ 数学的定義
#### 数列の総乗(product)

数列$\langle a_n \rangle$の第$i$項までの積を$p_i$とすると

> $p_0 = a_0$

> $p_i = p_{i-1} \times a_i$

総乗の記号$\prod$で表すと

> $\prod\limits_{i=0}^{l-1} a_i = p_{l-1} = a_0 \times a_1 \times a_2 \times ... \times a_{l-2} \times a_{l-1}$

> $\prod\emptyset = 1$


## ■ Java
```java
// Java

public static int product1(final List<Integer> ns) {
  int p = 1;
  for (int n : ns) {
    p *= n;
  }
  return p;
}

// Java 8以降
public static int product2(final List<Integer> ns) {
  return ns.stream()
    .reduce(1, (x, y) -> x * y);
}
```


## ■ JavaScript
```javascript
// JavaScript

var product1 = function (ns) {
  var i;
  var len = ns.length;
  var p = 1;

  for (i = 0; i < len; i += 1) {
    p *= ns[i];
  }
  return p;
};

// JavaScript 1.8以降
var product2 = function (ns) {
  return ns.reduce(function (x, y) {
    return x * y;
  }, 1);
};
```


## ■ CoffeeScript
```coffeescript
# CoffeeScript

product1 = (ns) ->
  ns.reduce((x, y) ->
    x * y
  , 1)
```


## ■ TypeScript
```typescript
// TypeScript

var product1 = function (ns: Array<number>): number {
  return ns.reduce((x, y) => {
    return x * y;
  }, 1);
};
```


## ■ Python
```python
# Python

def product1(ns):
    p = 1
    for n in ns:
        p *= n
    return p

def product2(ns):
    return functools.reduce(lambda x, y: x * y, ns, 1)

def product3(ns):
    return functools.reduce(operator.mul, ns, 1)
```


## ■ Haskell
```haskell
-- Haskell

product1 :: [Int] -> Int
product1 = foldl' (\x y -> x * y) 1

product2 :: [Int] -> Int
product2 = foldl' (*) 1

product3 :: [Int] -> Int
product3 = product
```


## ■ Clojure
```clojure
;; Clojure

(defn product1 [ns]
  (reduce (fn [x y] (* x y)) 1 ns))

(defn product2 [ns]
  (reduce #(* %1 %2) 1 ns))

(defn product3 [ns]
  (reduce * 1 ns))

(defn product4 [ns]
  (apply * ns))
```


## ■ Common Lisp
```lisp
;; Common Lisp

(defun product1 (ns)
  (reduce #'(lambda (x y) (* x y)) ns :initial-value 1))

(defun product2 (ns)
  (reduce #'* ns :initial-value 1))

(defun product3 (ns)
  (apply #'* ns))
```


## ■ Scheme
```scheme
;; Scheme

(define (product1 ns)
  (fold (lambda (x y) (* x y)) 1 ns))

(define (product2 ns)
  (fold * 1 ns))

(define (product3 ns)
  (apply * ns))
```


## ■ OCaml
```ocaml
(* OCaml *)

let product1 =
  List.fold_left (fun x y -> x * y) 1

let product2 =
  List.fold_left ( * ) 1
```


## ■ Erlang
```erlang
%% Erlang

product1(Ns) ->
  lists:foldl(fun(X, Y) -> X * Y end, 1, Ns).
```


## ■ Scala
```scala
// Scala

def product1(ns: List[Int]): Int = {
  ns.foldLeft(1)((x, y) => x * y)
}

def product2(ns: List[Int]): Int = {
  ns.foldLeft(1)(_ * _)
}

def product3(ns: List[Int]): Int = {
  ns.product
}
```


## ■ Ruby
```ruby
# Ruby

def product1(ns)
  ns.inject(1) { |x, y| x * y }
end

def product2(ns)
  ns.inject(1, :*)
end
```


## ■ Groovy
```groovy
// Groovy

static int product1(List<Integer> ns) {
  ns.inject(1) { x, y -> x * y }
}
```


## ■ Perl
```perl
# Perl

sub product1 {
  my ($ns_ref) = @_;

  my $p = 1;
  for my $n (@$ns_ref) {
    $p *= $n;
  }
  return $p;
}

sub product2 {
  my ($ns_ref) = @_;

  return List::Util::reduce { $a * $b } 1, @$ns_ref;
}

sub product3 {
  my ($ns_ref) = @_;

  return List::Util::product @$ns_ref;
}
```
