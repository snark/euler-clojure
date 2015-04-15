(defn threefivesum [size]
    (reduce +
        (filter (fn [n]
            (or
                (zero? (mod n 3))
                (zero? (mod n 5))
            )
        )
        (range size)
        )
    )
)

(println (threefivesum 1000))
