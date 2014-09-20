■ reduce関数の利用例: 要素の整数の積
==============================
## Java
```java
// Java
public static int productReduce(List<Integer> list) {
  return list.stream()
    .reduce(1, (x, y) -> x * y);
}
```


## JavaScript
```javascript
// JavaScript
var productReduce = function (list) {
  return list.reduce(function (x, y) {
    return x * y;
  }, 1);
};
```


## Python
```python
# Python
def product_reduce(lst):
    return functools.reduce(lambda x, y: x * y, lst, 1)

def product_reduce2(lst):
    return functools.reduce(operator.mul, lst, 1)
```


## Haskell
```haskell
-- Haskell
productReduce :: [Int] -> Int
productReduce = foldl' (\x y -> x * y) 1

productReduce' :: [Int] -> Int
productReduce' = foldl' (*) 1

productReduce'' :: [Int] -> Int
productReduce'' = product
```


## Ruby
```ruby
# Ruby
def product_reduce(list)
  list.inject(1) {|x, y| x * y}
end

def product_reduce2(list)
  list.inject(1, :*)
end
```


## Clojure
```clojure
;; Clojure
(defn product-reduce [lst]
  (reduce (fn [x y] (* x y)) 1 lst))

(defn product-reduce2 [lst]
  (reduce #(* %1 %2) 1 lst))

(defn product-reduce3 [lst]
  (reduce * 1 lst))
```


## Common Lisp
```lisp
;; Common Lisp
(defun produce-reduce (lst)
  (reduce #'(lambda (x y) (* x y)) lst :initial-value 1))

(defun product-reduce2 (lst)
  (reduce #'* lst :initial-value 1))
```


## Scala
```scala
// Scala
def productReduce(list: List[Int]): Int = {
  list.fold(1)((x, y) => x * y)
}

def productReduce2(list: List[Int]): Int = {
  list.fold(1)(_ * _)
}

def productReduce3(list: List[Int]): Int = {
  list.product
}
```


## Groovy
```groovy
// Groovy
static int productReduce(List<Int> list) {
  list.inject(1) {x, y -> x * y}
}
```


## Perl
```perl
# Perl
sub product_reduce {
  my ($list_ref) = @_;

  return List::Util::reduce {$a * $ b} 1, @$list_ref;
}

sub product_reduce2 {
  my ($list_ref) = @_;

  return List::Util::product @$list_ref;
}
```
