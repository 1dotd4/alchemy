(define-library (alchemy number-theory)
  (export double-and-add square-multiply
          gcd xgcd
          chinese-remainder-theorem
          sum-of-two-squares? prime? kronecker legendreSymbol tonelli phi
          order-of-element
          modexpt
          factors
          integer-square-root
          square-test prime-power-test
          ;;
          make-unit-group ZZn*
          )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy algebra)
          (srfi 1)
          (srfi 27))
  (begin
    (define (add1 n) (+ n 1))
    (define (sub1 n) (+ n -1))

    (define (make-unit-group n)
      (make-group (lambda (a) (and (integer? a) (= 1 (xgcd->d (xgcd a n)))))
                  (phi n)
                  1 (lambda (a b) (modulo (* a b) n)) (lambda (a) (xgcd->u (xgcd a n)))))

    (define (ZZn* n) (make-unit-group n))

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

    ;; 1.3.1
    (define original-gcd gcd)
    (define gcd
      (case-lambda
        ((a b) (original-gcd a b))
        ((R a b)
         (let rec ((a a) (b b))
           (if (r:zero? R b)
             a
             (rec b (r:modulo R a b)))))))


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

    ;; 1.4.3
    ;; in (Z/nZ)^*
    (define (order-of-element G g)
      ; G a group
      ; h cardinality of the group
      ; g an element of G
      ; 1 unit element of G
      (let* ((h (s:cardinality G))
            (fs (factors h)))
        (let rec ((e h) (rfs fs))
          (if (null? rfs) e
            (let* ((ee (/ e (expt (caar rfs) (cdar rfs))))
                  (g1 (double-and-add G g ee)))
              (let rec2 ((gg1 g1) (eee ee))
                (if (= 1 gg1)
                  (rec eee (cdr rfs))
                  (rec2 (double-and-add G gg1 (caar rfs))
                        (* eee (caar rfs))))))))))

    ;; 1.4.4
    (define (primitive-root p)
      (error "Will not found"))

    ;; 1.4.10
    (define (kronecker a b)
      (define (tab2k a)
        (list-ref '(0 1 0 -1 0 -1 0 1) (modulo a 8)))
      (if (zero? b)
        (if (not (= 1 (abs a))) 0 1)
        (if (and (even? a) (even? b))
          0
          (let remove2s-b ((v 0) (b b))
            (if (even? b)
              (remove2s-b (add1 v) (quotient b 2))
              (let ((k (if (even? v) 1 (tab2k a))))
              (let reciprocity ((k (if (< a 0) (- k) k))
                                (a a)
                                (b (if (< b 0) (- b) b)))
                (cond
                  ((and (zero? a) (> b 1)) 0)
                  ((and (zero? a) (= b 1)) k)
                  (else
                    (let remove2s-a ((v 0) (a a))
                      (if (even? a)
                        (remove2s-a (add1 v) (quotient a 2))
                        (let ((k (if (odd? v) (* k (tab2k b)) k)))
                          (reciprocity (* k (if (and (odd? (quotient a 2)) (odd? (quotient b 2))) -1 1))
                                       (modulo b (abs a))
                                       (abs a))))))))))))))

    ;; The Algorithm of Tonelly and Shanks

    ;; 1.5.1 - Tonelli Square root modulo
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

    ;; 1.6.1 - Solving Polynomial Equation Modulo p
    ; (define (roots-mod-p Fp P)
    ;   ;; p >= 3
    ;   ;; Fp = Z/pZ
    ;   ;; P in Fp[X]
    ;   ;; outputs the roots of P in Fp
    ;   (let rec ((A (gcd Fp (x^p - x) P)))
    ;             (roots '()))
    ;     (if (p:zero? A)
    ;       (rec (p:quot A X) (cons 0 roots))
    ;       (cond
    ;         ((zero? (p:eval A 0)) roots)
    ;         ((= 1 (p:deg A)) (cons -a0/a1 roots))
    ;         ((= 2 (p:deg A))
    ;          (let* ((d (a1 * a1 - 4 * a0 * a2a))
    ;                 (s (jacobi (d p))))
    ;            (if (= -1 s)
    ;              roots
    ;              (let ((e (tonelli d p)))
    ;                (append
    ;                  (list
    ;                    (/ (-a1 + e) (2*a2))
    ;                    (/ (-a1 - e) (2*a2)))
    ;                  roots)))))
    ;         (else
    ;           (let* step3 ((a (random in Fp)))
    ;             (let (B (gcd ((X + a)^{(p-1)/2}) A))
    ;               (if (or (zero? (p:deg B))
    ;                       (= (p:deg B) (p:deg A)))
    ;                 (step3 (random in Fp))
    ;                 (append
    ;                   (roots-mod-p Fp B)
    ;                   (roots-mod-p Fp (p:div A B))
    ;                   roots))))))))

    ;; 1.7.1
    (define (integer-square-root n)
      (let rec ((x n))
        (let ((y (quotient (+ x (quotient n x)) 2)))
          (if (< y x)
            (rec y)
            x))))

    ;; 1.7.3
    (define (square-test n)
      (let ((q11 (map (lambda (t) (>= (kronecker t 11) 0)) (iota 11)))
            (q63 (map (lambda (t) (>= (kronecker t 63) 0)) (iota 63)))
            (q64 (map (lambda (t) (>= (kronecker t 64) 0)) (iota 64)))
            (q65 (map (lambda (t) (>= (kronecker t 65) 0)) (iota 65))))
        (and
          (list-ref q64 (modulo n 64))
          (let ((r (modulo n 45045)))
            (and
              (list-ref q63 (modulo r 63))
              (list-ref q65 (modulo r 65))
              (list-ref q11 (modulo r 11))
              (let ((q (integer-square-root n)))
                (= n (* q q))))))))

    ;; 1.7.4
    (define (prime-power-test n)
      (let rec ((a 2))
        (let* ((b (square-multiply (ZZn n) a n))
               (p (gcd (- b a) n)))
          (cond
            ((= 1 p) #f)
            ((prime? p)
             (let try ((n* n))
               (cond
                 ((= 1 n*) p)
                 ((not (zero? (modulo n* p))) #f)
                 (else (try (quotient n* p))))))

            (else (rec (add1 a)))))))


    ;; =================================
    ;; n = a^2 + b^2 => n mod 4 === {0, 1, 2}
    (define (sum-of-two-squares? n)
      (member (modulo n 4) '(0 1 2)))
    ;; TODO: https://wstein.org/edu/2007/spring/ent/ent-html/node75.html

    ;; Legendre Symbol
    (define (legendreSymbol a p)
      (let ((power (modexpt a (quotient (- p 1) 2) p)))
        (if (> power 1) -1 power)))


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
          (if (= 1 number)
            factors
            (cons (cons number 1) factors))
          (if (zero? (modulo number divisor))
            (let count ((*number number) (i 0))
              (if (zero? (modulo *number divisor))
                (count (quotient *number divisor) (+ i 1))
                (*factor divisor *number (cons (cons divisor i) factors))))
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

