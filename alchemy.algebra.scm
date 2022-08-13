(define-library (alchemy algebra)
  (export make-set s:member? s:cardinality
          make-monoid make-group g:identity g:compose g:inverse
          g:identity?  g:got-inverse?
          make-ring r:zero r:add r:negate r:one r:multiply
          r:zero? r:one?
          make-euclidean-ring r:euclidean-division r:quotient r:modulo
          make-field f:inverse
          ring->multiplicative-monoid
          field->multiplicative-group
          ;;
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

    (define (g:compose a b c)
      ((list-ref a 4) b c))

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
    (define (make-ring member? cardinality zero add negate one multiply)
      (make-algebraic-structure member? cardinality
                                zero add negate
                                one multiply))

    (define (r:zero a)
      (list-ref a 3))

    (define (r:zero? a b)
      (equal? (list-ref a 3) b))

    (define (r:add a b c)
      ((list-ref a 4) b c))

    (define (r:negate a e)
      ((list-ref a 5) e))

    (define (r:one a)
      (list-ref a 6))

    (define (r:one? a b)
      (equal? (list-ref a 6) b))

    (define (r:multiply a b c)
      ((list-ref a 7) b c))
    
    (define (make-euclidean-ring member? cardinality
                                 zero add negate
                                 one multiply
                                 less?)
      (make-algebraic-structure member? cardinality
                                zero add negate
                                one multiply
                                less?))

    (define (r:less? r a b)
      ((list-ref r 8) a b))

    ;;; NOTE THAT THIS HERE IS INACCURATE
    (define (r:euclidean-division r a b)
      (let rec ((rem a) (divisor b) (quot (r:zero r)))
        (if (r:less? r rem divisor)
          (cons quot rem)
          (rec (r:add r rem (r:negate r b))
               divisor
               (r:add r quot (r:one r))))))

    (define (r:quotient r a b)
      (car (r:euclidean-division r a b)))

    (define (r:modulo r a b)
      (cdr (r:euclidean-division r a b)))

    ; XXX
    (define (ring->multiplicative-monoid ring)
      (apply make-monoid
             (append
               (take ring 2)
               (drop ring 6))))

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
               (drop field 6))))

    (define (f:inverse a b)
      ((list-ref a 9) b))

    ;;;; Lattice structure


    ;;;; Module-like structure


    ;;; Module

    ;;; Vector Space


    ;;;; Algebra-like structure

    ))
