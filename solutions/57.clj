(defn digit-count [n]
  (count (str n)))

(defn continue [previous current]
  (+' (*' 2 current) previous))

(defn continued-fraction [previous current]
  (cons current (lazy-seq (continued-fraction current (continue previous current)))))

(def numerators (continued-fraction 1 3))
(def denominators (continued-fraction 1 2))

(defn pairwise [n]
  (map vector (take n numerators) (take n denominators)))

(def answer (count (filter
         #(> (digit-count (first %)) (digit-count (last %)))
         (pairwise 1000))))

(println answer)
