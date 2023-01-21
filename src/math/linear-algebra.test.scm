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
        (alchemy linear-algebra))


(display (matrix->string (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12))))

(display (matrix->string (transpose (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12)))))

(display (matrix->string (ma-swap-col! (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12))
                     1 2)))

(display (matrix->string (rho 2 2 '(2 3 5 6))))
(display (vec->string
  (square-linear-system
    (rho 2 2 '(2 3 5 6))
    (vector 4 7))))

(display (vec->string
  (square-linear-system
    (rho 4 4 '( 1  1 -3  1
               -5  3 -4  1
                1  0  2 -1
                1  2  0  0))
    (vector 2 0 1 12))))

(display (matrix->string (matrix-identity 3)))

(display (matrix->string
  (matrix-inverse
    (rho 4 4 '( 1  1 -3  1
               -5  3 -4  1
                1  0  2 -1
                1  2  0  0)))))

(display (matrix->string
  (matrix-inverse
    (rho 2 2 '(4 7
               2 6)))))

(display "Should be I: \n")
(display (matrix->string
  (matrix-multiplication
    (matrix-inverse
      (rho 2 2 '(4 7 2 6)))
    (rho 2 2 '(4 7 2 6)))))

(display "Should be I: \n")
(display (matrix->string
  (matrix-multiplication
    (rho 2 2 '(6/10 -7/10 -2/10 4/10))
    (rho 2 2 '(4 7 2 6)))))

(display (matrix->string
  (matrix-inverse
    (rho 1 1 '(7)))))

(print
  (matrix-determinat
    (rho 1 1 '(7))))

(print
  (matrix-determinat
    (rho 2 2 '(2 3 4 5))))

(print
  (matrix-determinat
    (rho 3 3 '(2 3 4 5 6 7 8 9 0))))

(let ((res (matrix-kernel
             (rho 2 3 '(2 3 5 -4 2 3)))))
  (map (compose display vec->string) res))

(display "Gram schmidt 1:\n")
(print (gram-schmidt (list (vector 1 -1 1) (vector 1 0 1) (vector 1 1 2))))
;; (#(-1/2 0 1/2) #(1/3 2/3 1/3) #(1 -1 1))

; (print
;   (LLL
;     (rho 10 10 '(1 0 0 0 0 0 0 0 0 0
;                  0 1 0 0 0 0 0 0 0 0
;                  0 0 1 0 0 0 0 0 0 0
;                  0 0 0 1 0 0 0 0 0 0
;                  0 0 0 0 1 0 0 0 0 0
;                  0 0 0 0 0 1 0 0 0 0
;                  0 0 0 0 0 0 1 0 0 0
;                  0 0 0 0 0 0 0 1 0 0
;                  0 0 0 0 0 0 0 0 1 0
;                  575 436 1586 1030 1921 569 721 1183 1570 -6665))
;     3/4))


; (print
;   (gram-schmidt
;     (vector->list
;       (transpose
;         (rho 2 2
;              '(3 -2
;                2 -2))))))

; (steer-observe
;   "LLL on 2x2 matrix"
;   (LLL
;     (rho 2 2 '(3 -2
;                2 -2))
;     3/4)
;   (rho 2 2 '(-1 0 0 -2)))

(display "Gram schmidt 2:\n")
(print (gram-schmidt (list (vector 0 1 2)
                           (vector 1 -1 3)
                           (vector 1 2 6))))

(display
  (matrix->string
    (LLL
      (rho 3 3
          '(1 -1  3
            1  0  5
            1  2  6))
      3/4)))

(display
  (matrix->string
    (LLL
      (rho 5 5 '(1 0 0 0 -414
                0 1 0 0 -198
                0 0 1 0 -250
                0 0 0 1 -272
                0 0 0 0  884))
      3/4)))
