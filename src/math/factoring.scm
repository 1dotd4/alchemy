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

(define-library (alchemy factoring)
  (export 
    fermat-method
    )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy number-theory)
          )
  (begin

    (define (fermat-method n)
      (let iter ((t (integer-square-root n)))
        (let ((b (- (square t) n)))
          (if (square? b)
            (let ((c (integer-square-root b)))
              (cons (- t c) (+ t c)))
            (iter (+ t 1))))))

    ;; TODO:
    ;; - CFRAC method
    ;;   - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3985315/
    ;;   - https://web.math.pmf.unizg.hr/~duje/pdf/dujececc.pdf
    ;; - Class Group Method
    ;; - Elliptic Curve Method
    ;; - Multiple Polynomial Quadratic Sieve ?
    ;; - Number Field Sieve


    ;; End of module
    ))
