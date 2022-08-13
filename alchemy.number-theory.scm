(define-library (alchemy number-theory)
  (export double-and-add square-multiply
          xgcd
          chinese-remainder-theorem
          integer-ring Z
          make-integer-ring-modulo ZZn
    sum-of-two-squares? prime? legendreSymbol tonelli phi
          modexpt)
  (import (scheme base)
          (scheme write)
          (alchemy algebra)
          (srfi 27))
  (begin
    (define (add1 n) (+ n 1))
    (define (sub1 n) (+ n -1))

    (define integer-ring
      (make-ring integer?  'inf
        0 + - 1 * quotient <))

    (define Z integer-ring)

    (define (make-integer-ring-modulo n)
      (make-ring integer? (- n 1)
                 0 (lambda (a b) (modulo (+ a b) n)) (lambda (a) (- n a))
                 1 (lambda (a b) (modulo (* a b) n))
                 quotient <))

    (define (ZZn n) (make-integer-ring-modulo n))

    ;;;; From Cohen
    ;;; The Powering Algorithms
    ;; (G, *) a group.
    ;; recall group are closed under * and every element has an inverse.
    ;; We may also use monoid for free

    ;; 1.2.1
    (define (double-and-add algebraic-structure base exponent)
      (if (zero? exponent)
        (g:identity algebraic-structure)
        (if (< exponent 0)
          (if (g:got-inverse? algebraic-structure)
            (double-and-add (g:inverse algebraic-structure base) (- exponent))
            (error "Dear Cohen, no, I'll use a monoid."))
          (let rec ((result (g:identity algebraic-structure)) (exponent exponent) (composite base))
            (if (zero? exponent) result
              (if (odd? exponent)
                (rec (g:compose algebraic-structure composite result)
                     (quotient exponent 2) 
                     (g:compose algebraic-structure composite composite))
                (rec result
                     (quotient exponent 2)
                     (g:compose algebraic-structure composite composite))))))))

    (define (square-multiply algebraic-structure g n)
      (double-and-add (ring->multiplicative-monoid algebraic-structure) g n))

    ;; 1.3.6
    (define (xgcd ring a b) ; => (u, v, d)
      (if (r:zero? ring b)
        (list
          (r:one ring)
          (r:zero ring)
          a)
        (let rec ((u (r:one ring)) (d a) (v1 (r:zero ring)) (v3 b))
          (if (r:zero? ring v3)
            (list u
                  (r:quotient ring (r:subtract ring d (r:multiply ring a u)) b)
                  d)
            (let* ((q  (r:quotient ring d v3))
                   (t3 (r:modulo   ring d v3))
                   (t1 (r:subtract ring u (r:multiply ring q v1))))
              (rec v1 v3 t1 t3))))))

    (define (xgcd->u x) (car x))
    (define (xgcd->v x) (list-ref x 1))
    (define (xgcd->d x) (list-ref x 2))

    ;; 1.3.12
    ; note, gcd(mi, mj) = 1 for any pair.
    (define (chinese-remainder-theorem R xmis) ; (... (xi . mi) ...) => x ≡ xi mod mi for all i
      (let rec ((x (caar xmis)) (m (cdar xmis)) (rxmis (cdr xmis)))
        (if (null? rxmis) (cons x m)
          (let* ((xmi (car rxmis))
                 (xi (car xmi))
                 (mi (cdr xmi))
                 (r (xgcd R m mi))
                 (u (xgcd->u r))
                 (v (xgcd->v r))
                 (d (xgcd->d r)))
            (rec (r:modulo R
                           (r:add R (r:multiply R u m xi) (r:multiply R v mi x))
                           (r:multiply R m mi))
                 (r:multiply R m mi)
                 (cdr rxmis))))))

    ; ;;; Coninued Fraction Expression of Real Numbers
    ; ;; x = a0 + (1 / (a1 + (1 / (a2 + ...))))
    ; ;; x = [a0, a1, a2, ...]

    ; ;; 1.3.13
    ; (define (continue-fraction-lehmer x a a1) ; a < x < a1
    ;   ;; check a and a1 are exact as should be rational!
    ;   (error "okay no, write it when you need it"))

    ; ; 1.3.14
    ; (define (continue-fraction-gauss a b) ; a, b independent vectors in a Eucliedean vector space
    ;   (error "okay no, write it when you need it"))

    ; ;; The Legebdre Symbol

    ; ; in (Z/nZ)^*
    ; 
    ; ;; 1.4.3
    ; (define (order-of-element h g)
    ;   ; G a group
    ;   ; h cardinality of the group
    ;   ; g an element of G
    ;   ; 1 unit element of G
    ;   (let ((fs (factor h)))
    ;     (let rec ((e h) (rfs fs))
    ;       (if (null? rfs) e
    ;         (let ((ee (/ e (expt (caar rfs) (cdar rfs))))
    ;               (g1 (expt g ee)))
    ;           (let rec2 ((gg1 g1) (eee ee))
    ;             (if (= 1 gg1)
    ;               (rec ee (cdr rfs))
    ;               (rec2 (expt g1 (caar rfs))
    ;                     (* ee (caar rfs))))))))))

    ;; 1.4.4
    (define (primitive-root p)
      (error "Will not found"))

    ;; 1.4.10
    (define (kronecker a b)
      (error "Not today please"))


    ;; The Algorithm of Tonelly and Shanks

    ;; 1.5.1
    (define (square-root-mod-p a p)
      (error "copy me from somewhere else"))

    ;; =================================
    ;; n = a^2 + b^2 => n mod 4 === {0, 1, 2}
    (define (sum-of-two-squares? n)
      (member (modulo n 4) '(0 1 2)))
    ;; TODO: https://wstein.org/edu/2007/spring/ent/ent-html/node75.html

    ;; Legendre Symbol
    (define (legendreSymbol a p)
      (let ((power (modexpt a (quotient (- p 1) 2) p)))
      (if (> power 1)
          -1
          power)))

    ;; Tonelli Square root modulo
    (define (tonelli n p) 
      ;; Step 0. Check that n is indeed square: (n | p) must be ≡ 1
      (letrec ((check (lambda (a p)
                        (if (= (legendreSymbol a p) 1)
                            (setup a p)
                            #f)))
          

              ;; Setup
              ;; Step 1. Factors out powers of 2 from p-1
              ;; Define q -odd- and s such as p-1 = q * 2^s
              ;; Step 2. Select a non-square z such as (z | p) = -1
              ;; and set c ≡ z^q
              ;; Step 3. Set r ≡ n^((q+1)/2), t ≡ n^q, m = s
              (findQandS (lambda (p s)
                            (if (zero? (remainder p 2))
                                (findQandS (quotient p 2) (add1 s))
                                (cons p s))))
              (findz (lambda (n i)
                        (if (= -1 (legendreSymbol i n))
                            i
                            (findz n (add1 i)))))
              (setup (lambda (n p)
                        (let* ((qands (findQandS (sub1 p) 0))
                                (q (car qands))
                                (s (cdr qands)))
                          
                          (loop n
                                p
                                s
                                (modexpt (findz p 0) q p)
                                (modexpt n q p)
                                (modexpt n (quotient (add1 q) 2) p)))))
              

              ;; Step 4. Loop.
              ;; If t ≡ 1 output r, p-r.
              ;; n p m c t r
              ;; Otherwise find, by repeated squaring,
              ;;   the lowest i, 0 < i < m, such as t^(2^i) ≡ 1
              ;; Let b ≡ c^(2^(m-i-1)), and set r ≡ r*b,
              ;; t ≡ t*b^2, c ≡ b^2 and m = i.
              (lowesti (lambda (t p i)
                          (if (= 1 (modulo t p))
                              i
                              (lowesti (modexpt t 2 p) p (add1 i)))))
              (loop (lambda (n p m c t r)
                        (let* ((i (lowesti (modexpt t 2 p) p 1))
                              (b (modexpt c (expt 2
                                                  (if (> (- m i 1) 0)
                                                      (- m i 1)
                                                      0))
                                          p)))
                          (if (= 1 t)
                              (cons r (- p r))
                              (loop n
                                    p
                                    i
                                    (modulo (* b b)   p)
                                    (modulo (* t b b) p)
                                    (modulo (* r b)   p)))))))
      ;; Start the tonelli algorithm.
      (check n p)))

    ;; Note to self, get a factor function and simplify this trivial
    ;; division.
    (define (phi n)
      (define (fit-number a b)
        (define (f a b r)
          (if (zero? (quotient a b))
              r
              (if (zero? (remainder a b))
                  (f (quotient a b) b (+ 1 r))
                  r)))
        (f a b 0))
      (define (until-factored n i r)
        (if (<= i n)
          (if (zero? (remainder n i)) ;; aka divides
              (let ((k (fit-number n i)))
                (until-factored (quotient n (expt i k))
                                (+ 1 i)
                                (* r (- i 1) (expt i (sub1 k)))))
              (until-factored n (+ 1 i) r))
          r))
      (until-factored n 2 1))

    ;; ===[ Prime Numbers ]===
    (define (modexpt b e M)
      (cond
        ((zero? e) 1)
        ((even? e) (modexpt (modulo (* b b) M) (quotient e 2) M))
        ((odd? e) (modulo (* b (modexpt b (- e 1) M)) M))))

    (define (prime? n)
      (define (pseudoprime? n)
        (define (split n)
          (let recur ((s 0) (d n))
            (if (odd? d)
              (values s d)
              (recur (+ 1 s) (quotient d 2)))))
        (define (composite-witness? n a)
          (let*-values (((s d) (split (sub1 n)))
                        ((x)   (modexpt a d n)))
            (and (not (= x 1))
                 (not (= x (sub1 n)))
                 (let try ((r (sub1 s)) (x (modexpt x 2 n)))
                   (or (zero? r)
                       (= x 1)
                       (and (not (= x (sub1 n)))
                            (try (sub1 r) (modexpt x 2 n))))))))
        (let-values (((s d) (split (sub1 n))))
          (let rec ((a (+ 2 (random-integer (- n 2))))
                    (k s))
            (or (zero? k)
                (and (not (composite-witness? n a))
                     (rec (+ 2 (random-integer (- n 2))) (sub1 k)))))))
      (and (> n 1)
           (or (= n 2)
               (and (not (zero? (modulo n 2)))
                    (pseudoprime? n)))))

    (define (factors n)
      (let *factor ((divisor 2) (number n) (factors '()))
        (if (> (* divisor divisor) number)
          (cons number factors)
          (if (zero? (modulo number divisor))
            (*factor divisor (quotient number divisor) (cons divisor factors))
            (*factor (+ 1 divisor) number factors)))))
    (define (next-prime n)
      (let rec ((next (+ 1 n)))
        (if (prime? next)
          next
          (rec (+ 1 next)))))
    (define (nth-prime n)
      (let rec ((prime 2) (nth 1))
        (if (= n nth)
          prime
          (rec (next-prime prime) (+ 1 nth)))))))

