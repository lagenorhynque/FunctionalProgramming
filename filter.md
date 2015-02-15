■ filter関数の利用例: 偶数の抽出
==============================
## Java
```java
// Java
public static List<Integer> filterEven(final List<Integer> list) {
  return list.stream()
    .filter(x -> x % 2 == 0)
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript
var filterEven = function (list) {
  return list.filter(function (x) {
    return x % 2 === 0;
  });
};
```


## CoffeeScript
```coffeescript
# CoffeeScript
filterEven = (list) ->
  list.filter((x) ->
    x % 2 is 0
  )

filterEven2 = (list) ->
  (x for x in list when x % 2 is 0)
```


## Python
```python
# Python
def filter_even(lst):
    return list(filter(lambda x: x % 2 == 0, lst))

def filter_even2(lst):
    return [x for x in lst if x % 2 == 0]
```


## Haskell
```haskell
-- Haskell
filterEven :: [Int] -> [Int]
filterEven = filter (\x -> x `mod` 2 == 0)

filterEven' :: [Int] -> [Int]
filterEven' = filter even

filterEven'' :: [Int] -> [Int]
filterEven'' list = [x | x -> list, x `mod` 2 == 0]
```


## Clojure
```clojure
;; Clojure
(defn filter-even [lst]
  (filter (fn [x] (= (rem x 2) 0)) lst))

(defn filter-even2 [lst]
  (filter #(= (rem % 2) 0) lst))

(defn filter-even3 [lst]
  (filter even? lst))

(defn filter-even4 [lst]
  (for [x lst :when (= (rem x 2) 0)] x))
```


## Common Lisp
```lisp
;; Common Lisp
(defun filter-even (lst)
  (remove-if-not #'(lambda (x) (= (rem x 2) 0)) lst))

(defun filter-even2 (lst)
  (remove-if-not #'evenp lst))

(defun filter-even3 (lst)
  (loop for x in lst if (= (rem x 2) 0) collect x))
```


## OCaml
```ocaml
(* OCaml *)
let filter_even =
  List.filter (fun x -> x mod 2 = 0)
```


## Erlang
```erlang
%% Erlang
filter_even(List) ->
  lists:filter(fun(X) -> X rem 2 =:= 0 end, List).

filter_even2(List) ->
  [X || X <- List, X rem 2 =:= 0].
```


## Scala
```scala
// Scala
def filterEven(list: List[Int]): List[Int] = {
  list.filter(x => x % 2 == 0)
}

def filterEven2(list: List[Int]): List[Int] = {
  list.filter(_ % 2 == 0)
}

def filterEven3(list: List[Int]): List[Int] = {
  for (x <- list if x % 2 == 0) yield x
}
```


## Ruby
```ruby
# Ruby
def filter_even(list)
  list.select { |x| x % 2 == 0 }
end

def filter_even2(list)
  list.select { |x| x.even? }
end
```


## Groovy
```groovy
// Groovy
static List<Integer> filterEven(List<Integer> list) {
  list.findAll { x -> x % 2 == 0 }
}

static List<Integer> filterEven2(List<Integer> list) {
  list.findAll { it % 2 == 0 }
}
```


## Perl
```perl
# Perl
sub filter_even {
  my ($list_ref) = @_;

  return grep { $_ % 2 == 0 } @$list_ref;
}
```
