(def first-primes [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97])

(defn sqroot [x]
  (if (> x 0) (Math/sqrt x) 0))

; trial division-based factorization, based on the Rosetta Code example --
; using recur to avoid blowing out the stack.
(defn factor
  ([n]
   (factor n 2 ()))
  ([n k accumulate]
   (if (= 1 n)
     accumulate
     (if (= 0 (rem n k))
       (recur (quot n k) k (cons k accumulate))
       (recur n (inc k) accumulate)))))
