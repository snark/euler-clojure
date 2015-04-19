; This is really a math problem rather than a programming problem; we could
; solve it with pencil and paper. To recap:
;
; phi(1) == 1
; phi(p) == p-1 for prime p
; phi(p^k) == k-1*p-1
; phi(m*n) = phi(m) * phi(n) for coprime m, n
;
; So given our problem, we need to be looking for a squarefree number (adding
; squares "uses up" our space under 1000000 without improving our ratio) that
; uses the most low primes we can fit in (because p/phi(p) approaches 1 as
; p increases). So 2*3*5...

(load-file "./libs/prime.clj")

(def answer
  (loop [acc 1 primes first-primes]
    (if (> (* acc (first primes)) 1000000)
      acc
      (recur (* acc (first primes)) (rest primes)))))

(println answer)
