■ filter関数の利用例: 偶数の抽出
==============================
## Java
```java
// Java
public static List<Integer> evenFilter(List<Integer> list) {
  return list.stream()
    .filter(x -> x % 2 == 0)
    .collect(Collectors.toList());
}
```


## JavaScript
```javascript
// JavaScript
var evenFilter = function (list) {
  return list.filter(function (x) {
    return x % 2 === 0;
  });
};
```


## Python
```python
# Python
def even_filter(lst):
    return filter(lambda x: x % 2 == 0, lst)

def even_filter2(lst):
    return [x for x in lst if x % 2 == 0]
```


## Haskell
```haskell
-- Haskell
evenFilter :: [Int] -> [Int]
evenFilter = filter (\x -> x `mod` 2 == 0)

evenFilter' :: [Int] -> [Int]
evenFilter' = filter even

evenFilter'' :: [Int] -> [Int]
evenFilter'' list = [x | x -> list, x `mod` 2 == 0]
```


## Ruby
```ruby
# Ruby
def even_filter(list)
  list.select {|x| x % 2 == 0}
end

def even_filter2(list)
  list.select {|x| x.even?}
end
```


## Clojure
```clojure
;; Clojure
(defn even-filter [lst]
  (filter (fn [x] (= (rem x 2) 0)) lst))

(defn even-filter2 [lst]
  (filter #(= (rem % 2) 0) lst))

(defn even-filter3 [lst]
  (filter even? lst))

(defn even-filter4 [lst]
  (for [x lst :when (= (rem x 2) 0)] x))
```


## Common Lisp
```lisp
;; Common Lisp
(defun even-filter (lst)
  (remove-if-not #'(lambda (x) (= (rem x 2) 0)) lst))

(defun even-filter2 (lst)
  (remove-if-not #'evenp lst))

(defun even-filter3 (lst)
  (loop for x in lst if (= (rem x 2) 0) collect x))
```


## Scala
```scala
// Scala
def evenFilter(list: List[Int]): List[Int] = {
  list.filter(x => x % 2 == 0)
}

def evenFilter2(list: List[Int]): List[Int] = {
  list.filter(_ % 2 == 0)
}

def evenFilter3(list: List[Int]): List[Int] = {
  for (x <- list if x % 2 == 0) yield x
}
```


## Groovy
```groovy
// Groovy
static List<Int> evenFilter(List<Int> list) {
  list.findAll {x -> x % 2 == 0}
}

static List<Int> evenFilter2(List<Int> list) {
  list.findAll {it % 2 == 0}
}
```


## Perl
```perl
# Perl
sub even_filter {
  my ($list_ref) = @_;

  return grep {$_ % 2 == 0} @$list_ref;
}
```
