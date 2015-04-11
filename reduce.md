■ reduce関数の利用例: 要素の整数の積
==============================
## Java
```java
// Java

public static int reduceProduct1(final List<Integer> ns) {
  return ns.stream()
    .reduce(1, (x, y) -> x * y);
}
```


## JavaScript
```javascript
// JavaScript

var reduceProduct1 = function (ns) {
  return ns.reduce(function (x, y) {
    return x * y;
  }, 1);
};
```


## CoffeeScript
```coffeescript
# CoffeeScript

reduceProduct1 = (ns) ->
  ns.reduce((x, y) ->
    x * y
  , 1)
```


## TypeScript
```typescript
// TypeScript

var reduceProduct1 = function (ns: Array<number>): number {
  return ns.reduce((x, y) => {
    return x * y;
  }, 1);
};
```


## Python
```python
# Python

def reduce_product1(ns):
    return functools.reduce(lambda x, y: x * y, ns, 1)

def reduce_product2(ns):
    return functools.reduce(operator.mul, ns, 1)
```


## Haskell
```haskell
-- Haskell

reduceProduct1 :: [Int] -> Int
reduceProduct1 = foldl' (\x y -> x * y) 1

reduceProduct2 :: [Int] -> Int
reduceProduct2 = foldl' (*) 1

reduceProduct3 :: [Int] -> Int
reduceProduct3 = product
```


## Clojure
```clojure
;; Clojure

(defn reduce-product1 [ns]
  (reduce (fn [x y] (* x y)) 1 ns))

(defn reduce-product2 [ns]
  (reduce #(* %1 %2) 1 ns))

(defn reduce-product3 [ns]
  (reduce * 1 ns))

(defn reduce-product4 [ns]
  (apply * ns))
```


## Common Lisp
```lisp
;; Common Lisp

(defun reduce-product1 (ns)
  (reduce #'(lambda (x y) (* x y)) ns :initial-value 1))

(defun reduce-product2 (ns)
  (reduce #'* ns :initial-value 1))

(defun reduce-product3 (ns)
  (apply #'* ns))
```


## Scheme
```scheme
;; Scheme

(define (reduce-product1 ns)
  (fold (lambda (x y) (* x y)) 1 ns))

(define (reduce-product2 ns)
  (fold * 1 ns))

(define (reduce-product3 ns)
  (apply * ns))
```


## OCaml
```ocaml
(* OCaml *)

let reduce_product1 =
  List.fold_left (fun x y -> x * y) 1

let reduce_product2 =
  List.fold_left ( * ) 1
```


## Erlang
```erlang
%% Erlang

reduce_product1(Ns) ->
  lists:foldl(fun(X, Y) -> X * Y end, 1, Ns).
```


## Scala
```scala
// Scala

def reduceProduct1(ns: List[Int]): Int = {
  ns.foldLeft(1)((x, y) => x * y)
}

def reduceProduct2(ns: List[Int]): Int = {
  ns.foldLeft(1)(_ * _)
}

def reduceProduct3(ns: List[Int]): Int = {
  ns.product
}
```


## Ruby
```ruby
# Ruby

def reduce_product1(ns)
  ns.inject(1) { |x, y| x * y }
end

def reduce_product2(ns)
  ns.inject(1, :*)
end
```


## Groovy
```groovy
// Groovy

static int reduceProduct1(List<Integer> ns) {
  ns.inject(1) { x, y -> x * y }
}
```


## Perl
```perl
# Perl

sub reduce_product1 {
  my ($ns_ref) = @_;

  return List::Util::reduce { $a * $b } 1, @$ns_ref;
}

sub reduce_product2 {
  my ($ns_ref) = @_;

  return List::Util::product @$ns_ref;
}
```
