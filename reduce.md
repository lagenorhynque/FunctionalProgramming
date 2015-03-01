■ reduce関数の利用例: 要素の整数の積
==============================
## Java
```java
// Java

public static int reduceProduct1(final List<Integer> list) {
  return list.stream()
    .reduce(1, (x, y) -> x * y);
}
```


## JavaScript
```javascript
// JavaScript

var reduceProduct1 = function (list) {
  return list.reduce(function (x, y) {
    return x * y;
  }, 1);
};
```


## CoffeeScript
```coffeescript
# CoffeeScript

reduceProduct1 = (list) ->
  list.reduce((x, y) ->
    x * y
  , 1)
```


## Python
```python
# Python

def reduce_product1(lst):
    return functools.reduce(lambda x, y: x * y, lst, 1)

def reduce_product2(lst):
    return functools.reduce(operator.mul, lst, 1)
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

(defn reduce-product1 [lst]
  (reduce (fn [x y] (* x y)) 1 lst))

(defn reduce-product2 [lst]
  (reduce #(* %1 %2) 1 lst))

(defn reduce-product3 [lst]
  (reduce * 1 lst))

(defn reduce-product4 [lst]
  (apply * lst))
```


## Common Lisp
```lisp
;; Common Lisp

(defun reduce-product1 (lst)
  (reduce #'(lambda (x y) (* x y)) lst :initial-value 1))

(defun reduce-product2 (lst)
  (reduce #'* lst :initial-value 1))

(defun reduce-product3 (lst)
  (apply #'* lst))
```


## Scheme
```scheme
;; Scheme

(define (reduce-product1 lst)
  (reduce (lambda (x y) (* x y)) 1 lst))

(define (reduce-product2 lst)
  (reduce * 1 lst))

(define (reduce-product3 lst)
  (apply * lst))
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

reduce_product1(List) ->
  lists:foldl(fun(X, Y) -> X * Y end, 1, List).
```


## Scala
```scala
// Scala

def reduceProduct1(list: List[Int]): Int = {
  list.foldLeft(1)((x, y) => x * y)
}

def reduceProduct2(list: List[Int]): Int = {
  list.foldLeft(1)(_ * _)
}

def reduceProduct3(list: List[Int]): Int = {
  list.product
}
```


## Ruby
```ruby
# Ruby

def reduce_product1(list)
  list.inject(1) { |x, y| x * y }
end

def reduce_product2(list)
  list.inject(1, :*)
end
```


## Groovy
```groovy
// Groovy

static int reduceProduct1(List<Integer> list) {
  list.inject(1) { x, y -> x * y }
}
```


## Perl
```perl
# Perl

sub reduce_product1 {
  my ($list_ref) = @_;

  return List::Util::reduce { $a * $b } 1, @$list_ref;
}

sub reduce_product2 {
  my ($list_ref) = @_;

  return List::Util::product @$list_ref;
}
```
