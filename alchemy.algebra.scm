(define-library (alchemy algebra)
  (export 
          ;;
          )
  (import (scheme base)
          (scheme case-lambda)
          )
  (begin
    ;; Note: axioms are not proven here
    

    ;;;; Simple structure

    ;;; Set
    (define (make-set member?)
      (list
        'set
        member?))

    (define (set:member? set e)
      ((cadr set) e))

    ; sure and P = NP
    ; (define (set:cardinality set)
    ;   (caddr set))


    ;;;; Group-like structure

    ;;; Monoid
    (define (make-monoid member? identity compose)
      (list
        'monoid
        (make-set member?)
        identity
        compose))
    
    (define (monoid->set monoid)
      (list-ref monoid 1))

    (define (monoid:member? monoid e)
      (set:member? (monoid->set monoid) e))

    (define (monoid:identity monoid)
      (list-ref monoid 2))

    (define (monoid:compose monoid a b)
      ((list-ref monoid 3) a b))

    ;;; Group
    (define (make-group member? identity compose inverse)
      (list
        'group
        (make-monoid member? identity compose)
        inverse))

    (define (group->monoid group)
      (car group))

    (define (group->set group)
      (monoid->set (car group)))

    (define (group:member? group e)
      (monoid:member? (group->monoid group) e))

    (define (group:identity group)
      (monoid:identity (group->monoid group)))

    (define (group:compose group a b)
      (monoid:compose (group->monoid group) a b))

    (define (group:inverse group a)
      ((list-ref group 2) a))


    ;;;; Ring-like structure

    ;; https://mathstrek.blog/2012/10/31/introduction-to-ring-theory-8/

    ;;; Ring (group and monoid)
    (define make-ring
      (case-lambda
        ((additive-member? zero add negate multiplicative-member? one multiply)
         (make-ring additive-member? zero add negate multiplicative-member? one multiply
                    (lambda (a) (error "Multiplicative group not defined."))))
        ((additive-member? zero add negate multiplicative-member? one multiply inverse)
         (list 'ring
               (make-group additive-member?       zero add negate)
               (make-group multiplicative-member? one multiply inverse)))))

    (define (ring->additive-group ring)
      (list-ref ring 1))
  
    (define (ring:additive-member? ring e)
      (group:member? (ring->additive-group ring) e))

    (define (ring:zero ring)
      (group:identity (ring->additive-group ring)))

    (define (ring:add ring a b)
      (group:compose (ring->additive-group ring) a b))

    (define (ring:negate ring a)
      (group:inverse (ring->additive-group ring) a))

    (define (ring->multiplicative-group ring)
      (list-ref ring 2))

    (define (ring:multiplicative-member? ring e)
      (group:member? (ring->multiplicative-group ring) e))

    (define (ring:one ring)
      (group:identity (ring->multiplicative-group ring)))

    (define (ring:multiply ring a b)
      (group:compose (ring->multiplicative-group ring) a b))

    (define (ring:inverse ring a)
      (group:inverse (ring->multiplicative-group ring) a))


    ;;; Field (Two groups)
    ; Group<+> + Monoid<*>
    ; = Ring + inverse<*>

    ;;;; Lattice structure


    ;;;; Module-like structure


    ;;; Module

    ;;; Vector Space


    ;;;; Algebra-like structure

    ))
