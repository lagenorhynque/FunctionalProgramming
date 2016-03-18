type class & context bound
==========================

## concrete type
```haskell
-- Haskell

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

```scala
// Scala

def factorial(x: Int): Int = x match {
  case 0 => 1
  case n => n * factorial(n - 1)
}
```

## type class
```haskell
-- Haskell

factorial2 :: (Num a, Eq a) => a -> a
factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)
```

```scala
// Scala

def factorial2[T](x: T)(implicit numericOp: Numeric[T]): T = {
  val Zero = numericOp.zero
  x match {
    case Zero => numericOp.one
    case n => numericOp.times(n, factorial2(numericOp.minus(n, numericOp.one)))
  }
}
```

## context bound (syntactic sugar)
```scala
// Scala

def factorial3[T: Numeric](x: T): T = {
  val numericOp = implicitly[Numeric[T]]
  val Zero = numericOp.zero
  x match {
    case Zero => numericOp.one
    case n => numericOp.times(n, factorial3(numericOp.minus(n, numericOp.one)))
  }
}
```
