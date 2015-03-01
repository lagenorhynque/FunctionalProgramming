■ map関数の利用例: 整数の2乗
==============================
## Java
```java
// Java

public static List<Integer> mapSquare1(final List<Integer> list) {
  return list.steam()
    .map(x -> (int) Math.pow(x, 2))
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript

var mapSquare1 = function (list) {
  return list.map(function (x) {
    return Math.pow(x, 2);
  });
};
```


## CoffeeScript
```coffeescript
# CoffeeScript

mapSquare1 = (list) ->
  list.map((x) ->
    x ** 2
  )

mapSquare2 = (list) ->
  (x ** 2 for x in list)
```


## Python
```python
# Python

def map_square1(lst):
    return list(map(lambda x: x ** 2, lst))

def map_square2(lst):
    return [x ** 2 for x in lst]
```


## Haskell
```haskell
-- Haskell

mapSquare1 :: [Int] -> [Int]
mapSquare1 = map (\x -> x ^ 2)

mapSquare2 :: [Int] -> [Int]
mapSquare2 = map (^ 2)

mapSquare3 :: [Int] -> [Int]
mapSquare3 list = [x ^ 2 | x <- list]
```


## Clojure
```clojure
;; Clojure

(defn map-square1 [lst]
  (map (fn [x] (expt x 2)) lst))

(defn map-square2 [lst]
  (map #(expt % 2) lst))

(defn map-square3 [lst]
  (for [x lst] (expt x 2)))
```


## Common Lisp
```lisp
;; Common Lisp

(defun map-square1 (lst)
  (mapcar #'(lambda (x) (expt x 2)) lst))

(defun map-square2 (lst)
  (loop for x in lst collect (expt x 2)))
```


## Scheme
```scheme
;; Scheme

(define (map-square1 lst)
  (map (lambda (x) (expt x 2)) lst))
```


## OCaml
```ocaml
(* OCaml *)

let map_square1 =
  List.map (fun x -> int_of_float (float_of_int x ** 2.))
```


## Erlang
```erlang
%% Erlang

map_square1(List) ->
  lists:map(fun(X) -> trunc(math:pow(2, X)) end, List).

map_square2(List) ->
  [trunc(math:pow(2, X)) || X <- List].
```


## Scala
```scala
// Scala

def mapSquare1(list: List[Int]): List[Int] = {
  list.map(x => Math.pow(x, 2).toInt)
}

def mapSquare2(list: List[Int]): List[Int] = {
  list.map(Math.pow(_, 2).toInt)
}

def mapSquare3(list: List[Int]): List[Int] = {
  for (x <- list) yield Math.pow(x, 2).toInt
}
```


## Ruby
```ruby
# Ruby

def map_square1(list)
  list.collect { |x| x ** 2 }
end
```


## Groovy
```groovy
// Groovy

static List<Integer> mapSquare1(List<Integer> list) {
  list.collect { x -> Math.pow(x, 2) as int }
}

static List<Integer> mapSquare2(List<Integer> list) {
  list.collect { Math.pow(it, 2) as int }
}
```


## Perl
```perl
# Perl

sub map_square1 {
  my ($list_ref) = @_;

  return map { $_ ** 2 } @$list_ref;
}
```
