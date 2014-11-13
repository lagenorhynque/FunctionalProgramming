(ns fibonacci-number)

;; 再帰
(defn fibonacci1 [i]
  (cond (= i 0) 0N
        (= i 1) 1N
        :else   (+ (fibonacci1 (- i 2)) (fibonacci1 (- i 1)))))

;; 末尾再帰
(defn fibonacci2 [i]
  (letfn [(fib [n a b]
            (if (= n 0)
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
