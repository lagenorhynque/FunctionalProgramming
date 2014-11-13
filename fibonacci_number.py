# ループ
def fibonacci(i):
    a, b = 0, 1
    for n in range(i):
        a, b = b, a + b
    return a
