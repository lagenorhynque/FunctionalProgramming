■ filter関数の利用例: 偶数の抽出
==============================
## Java
```java
// Java

public static List<Integer> filterEven1(final List<Integer> ns) {
  return ns.stream()
    .filter(x -> x % 2 == 0)
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript

var filterEven1 = function (ns) {
  return ns.filter(function (x) {
    return x % 2 === 0;
  });
};
```


## CoffeeScript
```coffeescript
# CoffeeScript

filterEven1 = (ns) ->
  ns.filter((x) ->
    x % 2 is 0
  )

filterEven2 = (ns) ->
  (n for n in ns when n % 2 is 0)
```


## Python
```python
# Python

def filter_even1(ns):
    return list(filter(lambda x: x % 2 == 0, ns))

def filter_even2(ns):
    return [n for n in ns if n % 2 == 0]
```


## Haskell
```haskell
-- Haskell

filterEven1 :: [Int] -> [Int]
filterEven1 = filter (\x -> x `mod` 2 == 0)

filterEven2 :: [Int] -> [Int]
filterEven2 = filter even

filterEven3 :: [Int] -> [Int]
filterEven3 ns = [n | n <- ns, n `mod` 2 == 0]
```


## Clojure
```clojure
;; Clojure

(defn filter-even1 [ns]
  (filter (fn [x] (= (mod x 2) 0)) ns))

(defn filter-even2 [ns]
  (filter #(= (mod % 2) 0) ns))

(defn filter-even3 [ns]
  (filter even? ns))

(defn filter-even4 [ns]
  (for [n ns :when (= (mod n 2) 0)] n))
```


## Common Lisp
```lisp
;; Common Lisp

(defun filter-even1 (ns)
  (remove-if-not #'(lambda (x) (= (mod x 2) 0)) ns))

(defun filter-even2 (ns)
  (remove-if-not #'evenp ns))

(defun filter-even3 (ns)
  (loop for n in ns if (= (mod n 2) 0) collect n))
```


## Scheme
```scheme
;; Scheme

(define (filter-even1 ns)
  (filter (lambda (x) (= (mod x 2) 0)) ns))

(define (filter-even2 ns)
  (filter even? ns))
```


## OCaml
```ocaml
(* OCaml *)

let filter_even1 =
  List.filter (fun x -> x mod 2 = 0)
```


## Erlang
```erlang
%% Erlang

filter_even1(Ns) ->
  lists:filter(fun(X) -> X rem 2 =:= 0 end, Ns).

filter_even2(Ns) ->
  [N || N <- Ns, N rem 2 =:= 0].
```


## Scala
```scala
// Scala

def filterEven1(ns: List[Int]): List[Int] = {
  ns.filter(x => x % 2 == 0)
}

def filterEven2(ns: List[Int]): List[Int] = {
  ns.filter(_ % 2 == 0)
}

def filterEven3(ns: List[Int]): List[Int] = {
  for (n <- ns if n % 2 == 0) yield n
}
```


## Ruby
```ruby
# Ruby

def filter_even1(ns)
  ns.select { |x| x % 2 == 0 }
end

def filter_even2(ns)
  ns.select { |x| x.even? }
end
```


## Groovy
```groovy
// Groovy

static List<Integer> filterEven1(List<Integer> ns) {
  ns.findAll { x -> x % 2 == 0 }
}

static List<Integer> filterEven2(List<Integer> ns) {
  ns.findAll { it % 2 == 0 }
}
```


## Perl
```perl
# Perl

sub filter_even1 {
  my ($ns_ref) = @_;

  return grep { $_ % 2 == 0 } @$ns_ref;
}
```
