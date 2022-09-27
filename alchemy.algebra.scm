(define-library (alchemy algebra)
  (export make-set s:member? s:cardinality
          make-monoid make-group g:identity g:compose g:inverse
          g:identity?  g:got-inverse?
          make-ring r:zero r:add r:subtract r:negate r:one r:multiply
          r:zero? r:one?
          r:quotient r:modulo
          make-field f:inverse
          ring->multiplicative-monoid
          field->multiplicative-group
          ;;
          
          integer-ring Z
          make-integer-ring-modulo ZZn
          real-field R
          )
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          )
  (begin
    ;; Note: axioms are not proven here
    
    ;; TODO: easier coercion.

    (define (make-algebraic-structure . stuff)
      (cons 'algebraic-structure stuff))


    ;;;; Simple structure

    ;;; Set
    (define (make-set member? cardinality)
      (make-algebraic-structure member? cardinality))

    (define (s:member? a e)
      ((list-ref a 1) e))

    (define (s:cardinality a)
      (list-ref a 2))

    ;;;; Group-like structure

    ;;; Monoid
    (define (make-monoid member? cardinality identity compose)
      (make-algebraic-structure member? cardinality
                                identity compose))

    (define (g:identity a)
      (list-ref a 3))

    (define (g:identity? a b)
      (equal? (list-ref a 3) b))

    (define (g:compose G a . b)
      (fold (list-ref G 4) (g:identity G) (cons a b)))

    ;;; Group
    (define (make-group member? cardinality identity compose inverse)
      (make-algebraic-structure member? cardinality
                                identity compose inverse))

    (define (g:got-inverse? a)
      (> (length a) 5))

    (define (g:inverse a e)
      ((list-ref a 5) e))

    ;;;; Ring-like structure

    ;; https://mathstrek.blog/2012/10/31/introduction-to-ring-theory-8/

    ;;; Ring (group and monoid)
    (define (make-ring member? cardinality zero add negate one multiply quot less?)
      (make-algebraic-structure member? cardinality
                                zero add negate
                                one multiply quot less?))

    (define (r:zero R)
      (list-ref R 3))

    (define (r:zero? R b)
      (equal? (list-ref R 3) b))

    (define (r:add R a . b)
      (fold (list-ref R 4) (r:zero R) (cons a b)))

    (define (r:subtract R a . b)
      (fold (list-ref R 4) a (map (list-ref R 5) b)))

    (define (r:negate R e)
      ((list-ref R 5) e))

    (define (r:one R)
      (list-ref R 6))

    (define (r:one? a b)
      (equal? (list-ref a 6) b))

    (define (r:multiply R b . c)
      (fold (list-ref R 7) (r:one R) (cons b c)))

    (define (r:quotient R a b)
      ((list-ref R 8) a b))

    (define (r:modulo R a b)
      (r:subtract R a (r:multiply R b (r:quotient R a b))))
    
    ; (define (make-euclidean-ring member? cardinality
    ;                              zero add negate
    ;                              one multiply
    ;                              rem less?)
    ;   (make-algebraic-structure member? cardinality
    ;                             zero add negate
    ;                             one multiply
    ;                             rem less?))

    (define (r:less? r a b)
      ((list-ref r 9) a b))

    ; ;;; NOTE THAT THIS HERE IS INACCURATE
    ; (define (r:euclidean-division r a b)
    ;   (let rec ((rem a) (divisor b) (quot (r:zero r)))
    ;     (if (r:less? r rem divisor)
    ;       (cons quot rem)
    ;       (rec (r:add r rem (r:negate r b))
    ;            divisor
    ;            (r:add r quot (r:one r))))))


    ; XXX
    (define (ring->multiplicative-monoid ring)
      (apply make-monoid
             (append
               (take (drop ring 1) 2)
               (take (drop ring 6) 2))))

    ;;; Field (Two groups)
    ; Group<+> + Monoid<*>
    ; = Ring + inverse<*>
    (define (make-field member? cardinality zero add negate one multiply less? inverse)
      (make-algebraic-structure member? cardinality
                                zero add negate
                                one multiply less? inverse))

    (define (field->multiplicative-group field)
      (define (multiplicative-member? a)
        (if (not (equal? (g:identity field)))
          (s:member? field a)))
      (define multiplicative-cardinality
        (- (s:cardinality field) 1))
      (apply make-group
             (append
               (list multiplicative-member? 
                     multiplicative-cardinality)
               (take (drop field 6) 2))))

    (define (f:inverse a b)
      ((list-ref a 10) b))

    ;;;; Lattice structure


    ;;;; Module-like structure


    ;;; Module

    ;;; Vector Space

    (define (make-vector-space K V)
      ; K a field (but can be also a ring tbh)
      ; V the basis of our vector space
      (error "YOLO!"))


    ;;;; Algebra-like structure

    ;;;; Some known structures

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


    (define real-field
      (make-field number? 'inf
                  0 + - 1 * < (lambda (a) (/ 1 a))))

    (define R real-field)



    ))
