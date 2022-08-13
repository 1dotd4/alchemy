(define-library (alchemy algebra)
  (export make-set set? set:member? set:cardinality
          make-monoid monoid? monoid:member? monoid:cardinality monoid:identity monoid:compose
          monoid->set
          make-group group? group:member? group:cardinality group:identity group:compose
          group->monoid
          make-ring ring? ring:member? ring:cardinality ring:zero ring:add ring:negate ring:one ring:multiply
          ring->additive-group ring->multiplicative-monoid
          make-field field? field:member? field:cardinality field:zero field:add field:negate field:one
          field:multiply field:inverse field->additive-group field->multiplicative-group
          ;;
          )
  (import (scheme base)
          (scheme case-lambda)
          )
  (begin
    ;; Note: axioms are not proven here
    
    ;; TODO: easier coercion.

    ;;;; Simple structure

    ;;; Set
    (define (make-set member? cardinality)
      (list
        'set
        member?
        cardinality))

    (define (set? set)
      (equal? 'set (car set)))

    (define (set:member? set e)
      ((cadr set) e))

    (define (set:cardinality set)
      (list-ref set 2))


    ;;;; Group-like structure

    ;;; Monoid
    (define (make-monoid member? cardinality identity compose)
      (list
        'monoid
        (make-set member?)
        identity
        compose))

    (define (monoid? monoid)
      (equal? 'monoid (car monoid)))
    
    (define (monoid->set monoid)
      (list-ref monoid 1))

    (define (monoid:member? monoid e)
      (set:member? (monoid->set monoid) e))

    (define (monoid:cardinality monoid)
      (set:cardinality (monoid->set monoid)))

    (define (monoid:identity monoid)
      (list-ref monoid 2))

    (define (monoid:compose monoid a b)
      ((list-ref monoid 3) a b))

    ;;; Group
    (define (make-group member? cardinality identity compose inverse)
      (list
        'group
        (make-monoid member? cardinality identity compose)
        inverse))

    (define (group? group)
      (equal? 'group (car group)))

    (define (group->monoid group)
      (car group))

    (define (group->set group)
      (monoid->set (car group)))

    (define (group:member? group e)
      (monoid:member? (group->monoid group) e))

    (define (group:cardinality group)
      (monoid:cardinality (group->monoid group)))

    (define (group:identity group)
      (monoid:identity (group->monoid group)))

    (define (group:compose group a b)
      (monoid:compose (group->monoid group) a b))

    (define (group:inverse group a)
      ((list-ref group 2) a))


    ;;;; Ring-like structure

    ;; https://mathstrek.blog/2012/10/31/introduction-to-ring-theory-8/

    ;;; Ring (group and monoid)
    (define (make-ring member? cardinality zero add negate one multiply)
      (list 'ring
            (make-group member? cardinality zero add negate)
            one multiply))

    (define (ring? ring)
      (equal? 'ring (car ring)))

    (define (ring->additive-group ring)
      (list-ref ring 1))
  
    (define (ring:member? ring e)
      (group:member? (ring->additive-group ring) e))

    (define (ring:cardinality ring)
      (group:cardinality (ring->additive-group ring)))

    (define (ring:zero ring)
      (group:identity (ring->additive-group ring)))

    (define (ring:add ring a b)
      (group:compose (ring->additive-group ring) a b))

    (define (ring:negate ring a)
      (group:inverse (ring->additive-group ring) a))

    (define (ring:one ring)
      (list-ref ring 2))

    (define (ring:multiply ring a b)
      ((list-ref ring 3) a b))

    (define (ring:inverse ring a)
      ((list-ref ring 4) a))

    (define (ring->multiplicative-monoid ring)
      (make-monoid
        (ring:member? ring)
        (ring:cardinality ring)
        (ring:one ring)
        (ring:multiply ring)))


    ;;; Field (Two groups)
    ; Group<+> + Monoid<*>
    ; = Ring + inverse<*>
    (define (make-field member? cardinality zero add negate one multiply inverse)
      (list 'field
            (make-group member? cardinality zero add negate)
            inverse))
    
    (define (field? field)
      (equal? 'field (car field)))

    (define (field->ring field)
      (list-ref field 1))

    (define (field->additive-group ring)
      (list-ref ring 1))

    (define (field:member? field e)
      (ring:member? (field->ring field) e))

    (define (field:cardinality field)
      (ring:cardinality (field->ring field)))

    (define (field:zero field)
      (ring:zero (field->ring field)))

    (define (field:add field a b)
      (ring:add (field->ring field) a b))
    
    (define (field:negate field a)
      (ring:negate (field->ring field) a))

    (define (field:one field)
      (ring:one (field->ring field)))

    (define (field:multiply field a b)
      (ring:multiply (field->ring field) a b))
    
    (define (field:inverse field a)
      ((list-ref field 2) a))
    
    (define (field->multiplicative-group field)
      (define (multiplicative-member? a)
        (if (not (equal? (field:zero field)))
          (ring:member? (field->ring field) a)))
      (define multiplicative-cardinality
        (- (ring:cardinality (field->ring field)) 1))
      (make-group
        multiplicative-member? 
        multiplicative-cardinality
        (field:one field)
        (field:multiply field)
        (field:inverse field)))


    ;;;; Lattice structure


    ;;;; Module-like structure


    ;;; Module

    ;;; Vector Space


    ;;;; Algebra-like structure

    ))
