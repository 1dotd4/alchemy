(import
  (srfi-1)
  (chicken format))

;; J_n = {1, ..., n}
;; S_n = S(J_n) permutation of J_n
;; The vector (σ(1) ... σ(n)) is the permutation that takes each index i -> σ(i)

(define (permutation perm)
  (lambda (i) (list-ref perm (sub1 i))))

(define σ (permutation '(2 1 3)))

(define (pp perm)
  (print (map (o perm add1) (iota 3))))

(pp σ)

(define (orbit perm)
  (lambda (i)
    (let rec ((x i) (r (list i)))
      (let ((p (perm x)))
        (if (= i p)
          (reverse (cons i r))
          (rec p (cons p r)))))))

(print ((orbit σ) 2))

;; would be nice to have set theory here to disjoin stuff in orbits :3
