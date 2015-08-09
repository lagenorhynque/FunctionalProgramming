■ map関数の利用例: 整数の2乗
==============================
## Java
```java
// Java

public static List<Integer> mapSquare1(final List<Integer> ns) {
  return ns.steam()
    .map(x -> (int) Math.pow(x, 2))
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript

var mapSquare1 = function (ns) {
  return ns.map(function (x) {
    return Math.pow(x, 2);
  });
};
```


## CoffeeScript
```coffeescript
# CoffeeScript

mapSquare1 = (ns) ->
  ns.map((x) ->
    x ** 2
  )

mapSquare2 = (ns) ->
  (n ** 2 for n in ns)
```


## TypeScript
```typescript
// TypeScript

var mapSquare1 = function (ns: Array<number>): Array<number> {
  return ns.map((x) => {
    return Math.pow(x, 2);
  });
};
```


## Python
```python
# Python

def map_square1(ns):
    return list(map(lambda x: x ** 2, ns))

def map_square2(ns):
    return [n ** 2 for n in ns]
```


## Haskell
```haskell
-- Haskell

mapSquare1 :: [Int] -> [Int]
mapSquare1 = map (\x -> x ^ 2)

mapSquare2 :: [Int] -> [Int]
mapSquare2 = map (^ 2)

mapSquare3 :: [Int] -> [Int]
mapSquare3 ns = [n ^ 2 | n <- ns]
```


## Clojure
```clojure
;; Clojure

(defn map-square1 [ns]
  (map (fn [x] (expt x 2)) ns))

(defn map-square2 [ns]
  (map #(expt % 2) ns))

(defn map-square3 [ns]
  (for [n ns] (expt n 2)))
```


## Common Lisp
```lisp
;; Common Lisp

(defun map-square1 (ns)
  (mapcar #'(lambda (x) (expt x 2)) ns))

(defun map-square2 (ns)
  (loop for n in ns collect (expt n 2)))
```


## Scheme
```scheme
;; Scheme

(define (map-square1 ns)
  (map (lambda (x) (expt x 2)) ns))
```


## Scala
```scala
// Scala

def mapSquare1(ns: List[Int]): List[Int] = {
  ns.map(x => Math.pow(x, 2).toInt)
}

def mapSquare2(ns: List[Int]): List[Int] = {
  ns.map(Math.pow(_, 2).toInt)
}

def mapSquare3(ns: List[Int]): List[Int] = {
  for (n <- ns) yield Math.pow(n, 2).toInt
}
```


## Ruby
```ruby
# Ruby

def map_square1(ns)
  ns.collect { |x| x ** 2 }
end
```


## Groovy
```groovy
// Groovy

static List<Integer> mapSquare1(List<Integer> ns) {
  ns.collect { x -> Math.pow(x, 2) as int }
}

static List<Integer> mapSquare2(List<Integer> ns) {
  ns.collect { Math.pow(it, 2) as int }
}
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

map_square1(Ns) ->
  lists:map(fun(X) -> trunc(math:pow(2, X)) end, Ns).

map_square2(Ns) ->
  [trunc(math:pow(2, N)) || N <- Ns].
```


## Perl
```perl
# Perl

sub map_square1 {
  my ($ns_ref) = @_;

  return map { $_ ** 2 } @$ns_ref;
}
```
