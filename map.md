■ map関数の利用例: 整数の2乗
==============================
## Java
```java
// Java
public static List<Integer> mapSquare(final List<Integer> list) {
  return list.steam()
    .map(x -> (int) Math.pow(x, 2))
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript
var mapSquare = function (list) {
  return list.map(function (x) {
    return Math.pow(x, 2);
  });
};
```


## Python
```python
# Python
def map_square(lst):
    return map(lambda x: x ** 2, lst)

def map_square2(lst):
    return [x ** 2 for x in lst]
```


## Haskell
```haskell
-- Haskell
mapSquare :: [Int] -> [Int]
mapSquare = map (\x -> x ^ 2)

mapSquare' :: [Int] -> [Int]
mapSquare' = map (^ 2)

mapSquare'' :: [Int] -> [Int]
mapSquare'' list = [x ^ 2 | x <- list]
```


## Clojure
```clojure
;; Clojure
(defn map-square [lst]
  (map (fn [x] (expt x 2)) lst))

(defn map-square2 [lst]
  (map #(expt % 2) lst))

(defn map-square3 [lst]
  (for [x lst] (expt x 2)))
```


## Common Lisp
```lisp
;; Common Lisp
(defun map-square (lst)
  (mapcar #'(lambda (x) (expt x 2)) lst))

(defun map-square2 (lst)
  (loop for x in lst collect (expt x 2)))
```


## Scala
```scala
// Scala
def mapSquare(list: List[Int]): List[Int] = {
  list.map(x => Math.pow(x, 2).toInt)
}

def mapSquare2(list: List[Int]): List[Int] = {
  list.map(Math.pow(_, 2).toInt)
}

def mapSquare3(list: List[Int]): List[Int] = {
  for (x <- list) yield (Math.pow(x, 2).toInt)
}
```


## Erlang
```erlang
%% Erlang
map_square(List) ->
  lists:map(fun(X) -> trunc(math:pow(2, X)) end, List).

map_square2(List) ->
  [trunc(math:pow(2, X)) || X <- List].
```


## Ruby
```ruby
# Ruby
def map_square(list)
  list.collect {|x| x ** 2}
end
```


## Groovy
```groovy
// Groovy
static List<Int> mapSquare(List<Int> list) {
  list.collect {x -> Math.pow(x, 2) as int}
}

static List<Int> mapSquare2(List<Int> list) {
  list.collect {Math.pow(it, 2) as int}
}
```


## Perl
```perl
# Perl
sub map_square {
  my ($list_ref) = @_;

  return map {$_ ** 2} @$list_ref;
}
```
