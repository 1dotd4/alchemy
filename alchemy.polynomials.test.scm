(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy polynomials))

; (display (poly->string (make-poly R '(0 1 2))))
; (display (poly->string
;            (poly+
;              (make-poly R '(5 4 3 2))
;              (make-poly R '(0 1 2)))))

(steer-observe
  "Sum works."
  (make-poly R '(5 5 5 2))
  (poly+
    (make-poly R '(5 4 3 2))
    (make-poly R '(0 1 2))))

(steer-observe
  "Diff works."
  (make-poly R '(5 3 1 2))
  (poly-
    (make-poly R '(5 4 3 2))
    (make-poly R '(0 1 2))))

(steer-observe
  "Scalar works."
  (make-poly R '(5/2 2 3/2 1))
  (poly-by-scalar (make-poly R '(5 4 3 2)) 1/2))

; (display (poly->string
(display
  (poly->string
    (poly* (make-poly R '(1 3))
           (make-poly R '(1 0 5)))))

(display
  (poly->string
    (poly* (make-poly R '(1 3))
           (make-poly R '(0 5)))))

(display
  (poly->string
    (poly* (make-poly R '(1 3))
           (make-poly R '(1 2 5)))))

; (3x + 1) * (5x^2 + 1)
; = 15x^3 + 5x^2 + 3x + 1

; (3x + 1) * 5x = 15x^2 + 5x

; (3x + 1) * (5x^2 + 2x + 1)
; = 15x^3 + 6x^2 + 3x + 5x^2 + 2x + 1
; = 15x^3 + 11x^2 + 5x + 1

(display
  (poly->string
    (car
      (poly-euclidean-division
        (make-poly R '(-4 0 -2 1))
        (make-poly R '(-3 1))))))

(display
  (poly->string
    (car (poly-euclidean-division
      (make-poly R '(5/2 1))
      (make-poly R '(6))))))

(display
  (poly->string
    (poly-gcd
      (make-poly R '(0 1 1))
      (make-poly R '(6 1)))))

(display
  (poly->string
    (poly-gcd
      (make-poly R '(6 7 1))
      (make-poly R '(-6 -5 1)))))

