var Lazy = typeof require === "undefined" ? Lazy : require("lazy.js");

var FibonacciNumber = (function () {
  "use strict";

  // ループ
  var fibonacci = function (i) {
    var a = 0;
    var b = 1;
    var j;
    var tmp;

    for (j = 0; j < i; j += 1) {
      tmp = a;
      a = b;
      b += tmp;
    }
    return a;
  };

  // 再帰
  var fibonacci1 = function fibonacci1 (i) {
    if (i === 0) {
      return 0;
    } else if (i === 1) {
      return 1;
    } else {
      return fibonacci1(i - 2) + fibonacci1(i - 1);
    }
  };

  // 末尾再帰
  var fibonacci2 = function (i) {
    var fib = function fib (n, a, b) {
      if (n === 0) {
        return a;
      }
      return fib(n - 1, b, a + b);
    };

    return fib(i, 0, 1);
  };

  // 高階関数
  var fibonacci3 = function (i) {
    var fib = function (list) {
      return [list[1], Lazy(list).sum()];
    };

    return Lazy.range(0, i).reduce(fib, [0, 1])[0];
  };

  // 遅延評価
  var fibonacci4 = function (i) {
    var fibs = Lazy.generate(function () {
      var a = 0;
      var b = 1;

      return function () {
        var tmp = a;

        a = b;
        b += tmp;
        return a;
      };
    }(), i);

    return fibs.last();
  };

  var fibonacci5 = function (i) {
    // TODO
    return null;
  };

  // 行列の利用
  var fibonacci6 = function (i) {
    var prod = function (a, b) {
      return [[a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1]],
              [a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1]]];
    };
    var fibs = Lazy.generate(function () {
      var v = [[1, 0], [0, 1]];

      return function () {
        v = prod([[1, 1], [1, 0]], v);
        return v[0][1];
      };
    }(), i);

    return fibs.last();
  };

  var fibonacci6_2 = function (i) {
    var pow = function pow (f, x, n, a) {
      if (n === 0) {
        return a;
      } else if (n % 2 === 0) {
        return pow(f, f(x, x), n / 2, a);
      } else {
        return pow(f, x, n - 1, f(x, a));
      }
    };
    var prod = function (a, b) {
      return [[a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1]],
              [a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1]]];
    };

    return pow(prod, [[1, 1], [1, 0]], i, [[1, 0], [0, 1]])[0][1];
  };

  return {
    fibonacci: fibonacci,
    fibonacci1: fibonacci1,
    fibonacci2: fibonacci2,
    fibonacci3: fibonacci3,
    fibonacci4: fibonacci4,
    fibonacci5: fibonacci5,
    fibonacci6: fibonacci6,
    fibonacci6_2: fibonacci6_2
  };
}());

console.log(FibonacciNumber.fibonacci(10));
console.log(FibonacciNumber.fibonacci1(10));
console.log(FibonacciNumber.fibonacci2(10));
console.log(FibonacciNumber.fibonacci3(10));
console.log(FibonacciNumber.fibonacci4(10));
// console.log(FibonacciNumber.fibonacci5(10));
console.log(FibonacciNumber.fibonacci6(10));
console.log(FibonacciNumber.fibonacci6_2(10));

console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci).toArray());
console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci1).toArray());
console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci2).toArray());
console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci3).toArray());
console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci4).toArray());
// console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci5).toArray());
console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci6).toArray());
console.log(Lazy.range(0, 11).map(FibonacciNumber.fibonacci6_2).toArray());
