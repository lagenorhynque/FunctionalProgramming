■ map関数の利用例: 整数の2乗
==============================
## Java
```java
// Java
public static List<Integer> squareMap(List<Integer> list) {
  return list.steam()
    .map(x -> (int) Math.pow(x, 2))
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript
var squareMap = function (list) {
  return list.map(function (x) {
    return Math.pow(x, 2);
  });
};
```


## Python
```python
# Python
def square_map(lst):
    return map(lambda x: x ** 2, lst)

def square_map2(lst):
    return [x ** 2 for x in lst]
```


## Haskell
```haskell
-- Haskell
squareMap :: [Int] -> [Int]
squareMap = map (\x -> x ^ 2)

squareMap' :: [Int] -> [Int]
squareMap' = map (^ 2)

squareMap'' :: [Int] -> [Int]
squareMap'' list = [x ^ 2 | x <- list]
```


## Ruby
```ruby
# Ruby
def square_map(list)
  list.collect {|x| x ** 2}
end
```


## Clojure
```clojure
;; Clojure
(defn square-map [lst]
  (map (fn [x] (expt x 2)) lst))

(defn square-map2 [lst]
  (map #(expt % 2) lst))

(defn square-map3 [lst]
  (for [x lst] (expt x 2)))
```


## Common Lisp
```lisp
;; Common Lisp
(defun square-map (lst)
  (mapcar #'(lambda (x) (expt x 2)) lst))

(defun square-map2 (lst)
  (loop for x in lst collect (expt x 2)))
```


## Scala
```scala
// Scala
def squareMap(list: List[Int]): List[Int] = {
  list.map(x => Math.pow(x, 2).toInt)
}

def squareMap2(list: List[Int]): List[Int] = {
  list.map(Math.pow(_, 2).toInt)
}

def squareMap3(list: List[Int]): List[Int] = {
  for (x <- list) yield (Math.pow(x, 2).toInt)
}
```


## Groovy
```groovy
// Groovy
static List<Int> squareMap(List<Int> list) {
  list.collect {x -> Math.pow(x, 2) as int}
}

static List<Int> squareMap2(List<Int> list) {
  list.collect {Math.pow(it, 2) as int}
}
```


## Perl
```perl
# Perl
sub square_map {
  my ($list_ref) = @_;

  return map {$_ ** 2} @$list_ref;
}
```
