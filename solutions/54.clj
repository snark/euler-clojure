(def file (slurp "data/p054_poker.txt"))
;(def file (slurp "data/p054_dummy.txt"))

;Clojure has no native list indexing function, so let's build hashes
(def hand-ranks {:highcard 0, :pair 1, :twopair 2, :threeofkind 3, :straight 4, :flush 5, :fullhouse 6, :fourofkind 7, :straightflush 8})
(def face-ranks {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12})

(defn suit [cardtoken]
  (second cardtoken))

(defn rank [cardtoken]
  (get face-ranks (first cardtoken)))

(defn face-inc [m card]
  (update-in m [(first card)] (fnil inc 0)))

(defn card-comparer [handm x y]
  (compare [(get handm (first y)) (rank y)] [(get handm (first x)) (rank x)]))

(defn arrange-hand [hand]
  (let [face-count (reduce face-inc {} hand)]
  (sort (partial card-comparer face-count) hand)))

(defn is-flush? [hand]
  (= 1 (count (distinct (map suit hand)))))

; This ignores ace-low straights, which are irrelevant to the problem set
(defn is-straight? [hand]
    (= 4 (- (rank (first hand)) (rank (last hand)))))

(defn evaluate-hand [hand]
  (cond
    (= (rank (first hand)) (rank (nth hand 3))) :fourofkind
    (= (rank (nth hand 3)) (rank (nth hand 4))) :fullhouse
    (= (rank (first hand)) (rank (nth hand 2))) :threeofkind
    (= (rank (nth hand 2)) (rank (nth hand 3))) :twopair
    (= (rank (first hand)) (rank (nth hand 2))) :threeofkind
    (= (rank (first hand)) (rank (second hand))) :pair
    (and (is-straight? hand) (is-flush? hand)) :straightflush
    (is-flush? hand) :flush
    (is-straight? hand) :straight
    :else :highcard
  ))

(defn score-hands [player1 player2]
  (let [hand1 (arrange-hand player1)
        hand2 (arrange-hand player2)
        ranked1 (cons (get hand-ranks (evaluate-hand hand1)) (map rank hand1))
        ranked2 (cons (get hand-ranks (evaluate-hand hand2)) (map rank hand2))
        ]
    (compare (into [] ranked1) (into [] ranked2))))

(def line "5H 5C 6S 7S KD 2C 3S 8S 8D TD")
(def line2 "2C 3S 8S 8D TD 5H 5C 6S 7S KD")

(defn evaluate [cardstring]
  (let [cards (clojure.string/split cardstring #" ")]
    (if (= 1 (score-hands (take 5 cards) (drop 5 cards))) 1 0)))

(println (reduce + (map evaluate (clojure.string/split-lines file))))
