import java.util.Optional;

class OptionalInJava8 {
  /*
   * 整数a, bの最小公倍数を算出する。
   * a, bが自然数ではない場合、nullを返却する。
   */
  public static Integer unsafeGcd(int a, int b) {
    if (a < 1 || b < 1) {
      return null;
    }

    while (b > 0) {
      int r = a % b;
      a = b;
      b = r;
    }
    return a;
  }

  /*
   * 整数a, bの最小公倍数を算出する。
   * a, bが自然数ではない場合、Optional.emptyを返却する。
   */
  public static Optional<Integer> safeGcd(int a, int b) {
    if (a < 1 || b < 1) {
      return Optional.empty();
    }

    while (b > 0) {
      int r = a % b;
      a = b;
      b = r;
    }
    return Optional.of(a);
  }

  public static void main(final String[] args) {
    // unsafeGcd: nullチェックを怠るとNullPointerExceptionが発生する可能性がある
    // 1. 値を表示
    System.out.println(unsafeGcd(12, 18));
    System.out.println(unsafeGcd(12, -18));
    // 2. 値を2乗して表示
    System.out.println(Math.pow(unsafeGcd(12, 18), 2));
    // System.out.println(Math.pow(unsafeGcd(12, -18), 2));

    // safeGcd: Optionalに包まれた値を簡潔かつ安全に操作できる
    // 1-1. Optional自体を表示
    System.out.println(safeGcd(12, 18));
    System.out.println(safeGcd(12, -18));
    // 1-2. Optionalの中身の値を表示
    safeGcd(12, 18).ifPresent(System.out::println);
    safeGcd(12, -18).ifPresent(System.out::println);
    // 2. Optionalの中身の値を2乗して表示
    System.out.println(safeGcd(12, 18).map(gcd -> Math.pow(gcd, 2)));
    System.out.println(safeGcd(12, -18).map(gcd -> Math.pow(gcd, 2)));

    // Optional.ofNullableを利用すると、nullを返すメソッドの戻り値をOptionalでラップすることができる
    // 1-1. Optional自体を表示
    System.out.println(Optional.ofNullable(unsafeGcd(12, 18)));
    System.out.println(Optional.ofNullable(unsafeGcd(12, -18)));
    // 1-2. Optionalの中身の値を表示
    Optional.ofNullable(unsafeGcd(12, 18)).ifPresent(System.out::println);
    Optional.ofNullable(unsafeGcd(12, -18)).ifPresent(System.out::println);
    // 2. Optionalの中身の値を2乗して表示
    System.out.println(Optional.ofNullable(unsafeGcd(12, 18)).map(gcd -> Math.pow(gcd, 2)));
    System.out.println(Optional.ofNullable(unsafeGcd(12, -18)).map(gcd -> Math.pow(gcd, 2)));
  }
}
