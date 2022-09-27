(define-library (alchemy language)
  (export 
    define-curried
    compose
    ; compose-n
    applify
    a:map
    a:fold
    )
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 1)
          (srfi 232)
          )

  (begin

    ;;;; Backus, Curry et al.

    ;; Scheme has already the primitive functions to work but lacks of flexible forms.
    ;; We introduce here functional forms as macros for our programs.

    ; (define compose
    ;   (case-lambda
    ;     ((f g)
    ;      (lambda args
    ;        (call-with-values
    ;          (lambda ()
    ;            (apply g args))
    ;          f)))
    ;     ((f . g)
    ;      (lambda args
    ;        (call-with-values
    ;          (lambda ()
    ;            (apply (apply compose g) args))
    ;          f)))))

    (define (compose . fns)
      (define (binary-composition f g)
        (lambda args
          (call-with-values
            (lambda () (apply g args))
            f)))
      (define identity (lambda x (apply values x)))
      (fold-right binary-composition identity fns))

    ;; 1. composition
    ; (f o g):x = f:(g:x)
    ; (define compose-n
    ;   (case-lambda
    ;     ((f g)
    ;      (lambda args
    ;        (f (apply g args))))
    ;     ))
  ;       ((f . fns)
  ;         (lambda args
  ;           (f ((apply compose-n fns) args))))))

    ;; 2. construction
    ; [f1, ..., fn]:x = <f1:x, ..., fn:x>
    ; Since <..., _|_, ...> = _|_ and all functions are _|_-preserving, so is [f1, ..., fn]
    ; To me this looks like a contra-apply-to-all

    ;; 3. condition
    ; We already have if and it works fine

    ;; 4. constant
    ; k, bye

    ;; 5. insert - aka fold
    ; shall we just use fold from srfi-1 or ?
    (define-curried (a:fold fn init ls) (fold fn init ls))

    ;; 6. apply to all - aka better map
    (define-curried (a:map fn ls) (map fn ls))
    ; (define-curried (a:map fn . ls) (apply map fn ls))

    ;; 7. binary to unary - aka currying
    ; see define-curried

    ;; 8. while
    ; already have

    (define applify
      (lambda args
        (lambda x
          (apply apply (append args x)))))

    ))
