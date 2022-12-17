;; This file is part of alchemy.
;; Copyright (c) 2022 unpx.net
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy polynomials)
        (srfi 95))



(display
  (mpoly->string
    (make-multivariate-poly R '((1 1 2 100)
                                (-1 2 1 1)))))

(display
  (mpoly->string
    (mpoly+
      (make-multivariate-poly R '((1 1 2 10) (-1 2 1 1)))
      (make-multivariate-poly R '((-1 1 2 10) (7 2 3 4))))))

(steer-observe
  "Multivariate monomial order <glex."
  (sort '((1 1 2 100) (-1 1 3 1)) mpoly-<glex)
  '((-1 1 3 1) (1 1 2 100)))

(steer-observe
  "Multivariate monomial order <lex."
  (sort '((1 1 2 100) (-1 1 3 1)) mpoly-<lex)
  '((1 1 2 100) (-1 1 3 1)))

(exit)

(define F16 (ZZn 16))
;; (display
;;   (map
;;     (lambda (x) (evaluate-polynomial (make-poly F16 '(1 1 0 0 1)) x))
;;     (range 0 16)))
;; (display "\n")

(steer-observe
  "Lagrange interpolation."
  (lagrange-interpolation '((0 . 2) (1 . 3) (2 . 12) (5 . 147)) 3)
  35) ; f(3) = 35

(steer-observe
  "Polynomial evaluation."
  (evaluate-polynomial (make-poly R '(-1 2 -6 2)) 3)
  5)

;; TODO: make those tests
;; (display
;;   (poly->string
;;     (poly* (make-poly R '(1 3))
;;            (make-poly R '(1 2 5)))))
;; (display
;;   (poly->string
;;     (poly* (make-poly R '(1 3))
;;            (make-poly R '(0 5)))))
;; 
;; (display
;;   (poly->string
;;     (poly* (make-poly R '(1 3))
;;            (make-poly R '(1 0 5)))))
;; 
;; ; (3x + 1) * (5x^2 + 1)
;; ; = 15x^3 + 5x^2 + 3x + 1
;; 
;; ; (3x + 1) * 5x = 15x^2 + 5x
;; 
;; ; (3x + 1) * (5x^2 + 2x + 1)
;; ; = 15x^3 + 6x^2 + 3x + 5x^2 + 2x + 1
;; ; = 15x^3 + 11x^2 + 5x + 1
;; 
;; (display
;;   (poly->string
;;     (car
;;       (poly-euclidean-division
;;         (make-poly R '(-4 0 -2 1))
;;         (make-poly R '(-3 1))))))
;; 
;; (display
;;   (poly->string
;;     (car (poly-euclidean-division
;;       (make-poly R '(5/2 1))
;;       (make-poly R '(6))))))
;; 
;; (display
;;   (poly->string
;;     (poly-gcd
;;       (make-poly R '(0 1 1))
;;       (make-poly R '(6 1)))))
;; 
;; (display
;;   (poly->string
;;     (poly-gcd
;;       (make-poly R '(6 7 1))
;;       (make-poly R '(-6 -5 1)))))



(steer-observe
  "Scalar works."
  (make-poly R '(5/2 2 3/2 1))
  (poly-by-scalar (make-poly R '(5 4 3 2)) 1/2))

; (display (poly->string (make-poly R '(0 1 2))))
; (display (poly->string
;            (poly+
;              (make-poly R '(5 4 3 2))
;              (make-poly R '(0 1 2)))))

(steer-observe
  "Diff works."
  (make-poly R '(5 3 1 2))
  (poly-
    (make-poly R '(5 4 3 2))
    (make-poly R '(0 1 2))))

(steer-observe
  "Sum works."
  (make-poly R '(5 5 5 2))
  (poly+
    (make-poly R '(5 4 3 2))
    (make-poly R '(0 1 2))))

