(ns fibonacci-number)

;; 再帰
(defn fibonacci1 [i]
  (cond (= i 0) 0N
        (= i 1) 1N
        :else   (+ (fibonacci1 (- i 2)) (fibonacci1 (- i 1)))))

;; 末尾再帰
(defn fibonacci2 [i]
  (letfn [(fib [n a b]
            (if (zero? n)
              a
              (recur (dec n) b (+ a b))))]
    (fib i 0N 1N)))

;; 高階関数
(defn fibonacci3 [i]
  (letfn [(fib [[a b] _]
            [b (+ a b)])]
    (first (reduce fib [0N 1N] (range 0 i)))))

;; 遅延評価
(defn fibonacci4 [i]
  (let [fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N]))]
    (nth fibs i)))

(defn fibonacci5 [i]
  (letfn [(fibs [a b] (cons a (lazy-seq (fibs b (+ a b)))))]
    (nth (fibs 0N 1N) i)))

;; 行列の利用
(defn fibonacci6 [i]
  (letfn [(prod [[a11 a12 a21 a22] [b11 b12 b21 b22]]
            [(+ (* a11 b11) (* a12 b21)) (+ (* a11 b12) (* a12 b22))
             (+ (* a21 b11) (* a22 b21)) (+ (* a21 b12) (* a22 b22))])]
    (nth (nth (iterate #(prod [1N 1N 1N 0N] %) [1N 0N 0N 1N]) i) 1)))

(defn fibonacci6' [i]
  (letfn [(pow [f x n a]
            (cond
              (zero? n) a
              (even? n) (recur f (f x x) (quot n 2) a)
              :else     (recur f x (dec n) (f x a))))
          (prod [[a11 a12 a21 a22] [b11 b12 b21 b22]]
            [(+ (* a11 b11) (* a12 b21)) (+ (* a11 b12) (* a12 b22))
             (+ (* a21 b11) (* a22 b21)) (+ (* a21 b12) (* a22 b22))])]
    (nth (pow prod [1N 1N 1N 0N] i [1N 0N 0N 1N]) 1)))
