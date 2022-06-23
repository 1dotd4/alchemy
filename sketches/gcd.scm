

;; Generic GCD
;; Requires a, b in Euclidean Domain (special ring)
;; so which is best?

;; modulo zero?
(define (generate-greatest-common-divisor mod z?)
  (lambda (a b)
    (let rec ((x a) (y b))
      (if (z? y)
        x
        (rec y (mod x y))))))

;; equals? less? minus
(define (generate-greatest-common-divisor2 e? less? minus)
  (lambda (x y)
    (let rec ((a x) (b y))
      (if (e? a b)
        a
        (if (less? a b)
          (rec a (minus b a))
          (rec (minus a b) b))))))

(define (test fn . args)
  (define g (apply fn args))
  (print (g 33 77))
  (print (g 49865 69811)))


;; alternative to modulo to show the real requirements of GCD.
(define (mod a b)
  (if (< a 0)
    (mod (+ a b) b)
    (if (< a b)
      a
      (mod (- a b) b))))


(test generate-greatest-common-divisor mod zero?)
(test generate-greatest-common-divisor2 = < -)

