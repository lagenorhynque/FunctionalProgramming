Lazy = if require? then require("lazy.js") else Lazy

FibonacciNumber = do ->
  # ループ
  fibonacci = (i) ->
    [a, b] = [0, 1]

    for n in [0...i]
      [a, b] = [b, a + b]
    a

  # 再帰
  fibonacci1 = (i) ->
    if i is 0
      0
    else if i is 1
      1
    else
      fibonacci1(i - 2) + fibonacci1(i - 1)

  # 末尾再帰
  fibonacci2 = (i) ->
    fib = (n, a, b) ->
      if n is 0
        a
      else
        fib(n - 1, b, a + b)

    fib(i, 0, 1)

  # 高階関数
  fibonacci3 = (i) ->
    fib = ([a, b]) ->
      [b, a + b]

    Lazy.range(0, i).reduce(fib, [0, 1])[0]

  # 遅延評価
  fibonacci4 = (i) ->
    fibs = Lazy.generate(do ->
      [a, b] = [0, 1]

      ->
        [a, b] = [b, a + b]
        a
    , i)

    fibs.last()

  fibonacci5 = (i) ->
    # TODO
    null

  # 行列の利用
  fibonacci6 = (i) ->
    prod = ([a11, a12, a21, a22], [b11, b12, b21, b22]) ->
      [a11 * b11 + a12 * b21, a11 * b12 + a12 * b22,
       a21 * b11 + a22 * b21, a21 * b12 + a22 * b22]

    fibs = Lazy.generate(do ->
      v = [1, 0, 0, 1]

      ->
        v = prod([1, 1, 1, 0], v)
        v[1]
    , i)

    fibs.last()

  fibonacci6_2 = (i) ->
    pow = (f, x, n, a) ->
      if n is 0
        a
      else if n % 2 is 0
        pow(f, f(x, x), n / 2, a)
      else
        pow(f, x, n - 1, f(x, a))

    prod = ([a11, a12, a21, a22], [b11, b12, b21, b22]) ->
      [a11 * b11 + a12 * b21, a11 * b12 + a12 * b22,
       a21 * b11 + a22 * b21, a21 * b12 + a22 * b22]

    pow(prod, [1, 1, 1, 0], i, [1, 0, 0, 1])[1]

  {
    fibonacci: fibonacci,
    fibonacci1: fibonacci1,
    fibonacci2: fibonacci2,
    fibonacci3: fibonacci3,
    fibonacci4: fibonacci4,
    fibonacci5: fibonacci5,
    fibonacci6: fibonacci6,
    fibonacci6_2: fibonacci6_2
  }

console.log FibonacciNumber.fibonacci(10)
console.log FibonacciNumber.fibonacci1(10)
console.log FibonacciNumber.fibonacci2(10)
console.log FibonacciNumber.fibonacci3(10)
console.log FibonacciNumber.fibonacci4(10)
# console.log FibonacciNumber.fibonacci5(10)
console.log FibonacciNumber.fibonacci6(10)
console.log FibonacciNumber.fibonacci6_2(10)

console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci).toArray()
console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci1).toArray()
console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci2).toArray()
console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci3).toArray()
console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci4).toArray()
# console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci5).toArray()
console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci6).toArray()
console.log Lazy.range(0, 11).map(FibonacciNumber.fibonacci6_2).toArray()
