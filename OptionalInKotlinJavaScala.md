Optional in Kotlin, Swift, Java, and Scala
==========================================

## Kotlin
```kotlin
fun half(n: Int): Int? {
  return if (n % 2 == 0) n / 2 else null
}

println(half(2) ?: "undefined")  // 1
println(half(3) ?: "undefined")  // undefined
println(half(4) ?: "undefined")  // 2

half(2)?.let { "$it is a half of ${it * 2}" }  // => 1 is a half of 2
half(3)?.let { "$it is a half of ${it * 2}" }  // => null
half(4)?.let { "$it is a half of ${it * 2}" }  // => 2 is a half of 4
```

## Swift
```swift
func half(n: Int) -> Int? {
  return n % 2 == 0 ? n / 2 : nil
}

print(half(2) ?? "undefined")  // 1
print(half(3) ?? "undefined")  // undefined
print(half(4) ?? "undefined")  // 2

half(2).map { "\($0) is a half of \($0 * 2)" }  // => Optional("1 is a half of 2")
half(3).map { "\($0) is a half of \($0 * 2)" }  // => nil
half(4).map { "\($0) is a half of \($0 * 2)" }  // => Optional("2 is a half of 4")
```

## Java
```java
public static Optional<Integer> half(int n) {
  return n % 2 == 0 ? Optional.of(n / 2) : Optional.empty();
}

System.out.println(half(2).map(String::valueOf).orElse("undefined"));  // 1
System.out.println(half(3).map(String::valueOf).orElse("undefined"));  // undefined
System.out.println(half(4).map(String::valueOf).orElse("undefined"));  // 2

half(2).map(n -> String.format("%s is a half of %s", n, n * 2));  // => Optional[1 is a half of 2]
half(3).map(n -> String.format("%s is a half of %s", n, n * 2));  // => Optional.empty
half(4).map(n -> String.format("%s is a half of %s", n, n * 2));  // => Optional[2 is a half of 4]
```

## Scala
```scala
def half(n: Int): Option[Int] = if (n % 2 == 0) Some(n / 2) else None

println(half(2).map(_.toString).getOrElse("undefined"))  // 1
println(half(3).map(_.toString).getOrElse("undefined"))  // undefined
println(half(4).map(_.toString).getOrElse("undefined"))  // 2

half(2).map(n => s"$n is a half of ${n * 2}")  // => Some(1 is a half of 2)
half(3).map(n => s"$n is a half of ${n * 2}")  // => None
half(4).map(n => s"$n is a half of ${n * 2}")  // => Some(2 is a half of 4)
```
