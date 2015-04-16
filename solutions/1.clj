(defn threefive-sum [size]
  (->> (range size)
       (filter #(or (zero? (mod % 3))
                    (zero? (mod % 5))))
       (reduce +)))

(println (threefivesum 1000))

