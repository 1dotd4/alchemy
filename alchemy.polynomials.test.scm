(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy polynomials))

; (display (poly->string (make-poly R '(0 1 2))))
(display (poly->string
           (poly+
             (make-poly R '(5 4 3 2))
             (make-poly R '(0 1 2)))))

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
