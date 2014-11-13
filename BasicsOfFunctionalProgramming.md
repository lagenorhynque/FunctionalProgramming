Basics of Functional Programming
================================

## 1. list manipulation (リスト操作)

(:)/cons, head/first, tail/rest

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```

## 2. list comprehension (リスト内包表記)

generator, guard

```haskell
-- Haskell
pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples =
  [(m ^ 2 - n ^ 2, 2 * m * n, m ^ 2 + n ^ 2) |
    m <- [1..], n <- [1..(m - 1)], gcd m n == 1, odd (m - n)]
```
```clojure
;; Clojure
(defn pythagorean-triples []
  (for [m (iterate inc 1)
        n (range 1 m)
        :when (and (== (gcd m n) 1)
                   (odd? (- m n)))]
    [(- (expt m 2) (expt n 2)) (* 2 m n) (+ (expt m 2) (expt n 2))]))
```
```java
// Java

```

## 3. pattern matching (パターンマッチング)

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```

## 4. recursion (再帰)

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```

## 5. tail recursion (末尾再帰)

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```

## 6. partial application (部分適用), currying (カリー化) & function composition (関数合成)

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```

## 7. higher-order function (高階関数)

filter, map, fold/reduce

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```

## 8. lazy evaluation (遅延評価)

```haskell
-- Haskell

```
```clojure
;; Clojure

```
```java
// Java

```
