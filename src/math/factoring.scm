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
    lenstra-method
    )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy language)
          (alchemy algebra)
          (alchemy number-theory)
          (srfi 216)
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
    ;; - CFRAC method "smart fermat"
    ;; - Wiener, just for fun and RSA
    ;;   - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3985315/
    ;;   - https://web.math.pmf.unizg.hr/~duje/pdf/dujececc.pdf
    ;; - Dixon  <- requires linear algebra for kernel of a matrix
    ;; - Pollard p-1
    ;; - Pollard rho
    ;; - Elliptic Curve Method
    ;; - General Number Field Sieve

    ;; discrete logs?


    (define (lenstra-method n)
      (define (try-add P Q n A)
        (define (compute-lambda)
          ;;; Returns a list in case a factor is found, otherwise returns lambda
          (if (and (= (car P) (car Q)) (= (cdr P) (cdr Q)))
            ; same point addition
            (let ((denom (modulo (* 2 (cdr P)) n)))
              (if (zero? denom)
                (list (* 2 (cdr A)))
                (let ((x (xgcd Z denom n)))
                  (if (> (xgcd->d x) 1)
                    (if (= (xgcd->d x) n)
                      '()
                      (list (xgcd->d x)))
                    (modulo (* (xgcd->u x) (+ (* 3 (car P)) A)) n)))))
            (if (and (= (car P) (car Q)) (= (cdr P) (- (cdr Q))))
              '() ;; case we try to sum opposites
              (let ((denom (modulo (- (car Q) (car P)) n)))
                (if (zero? denom)
                  (list (- (car Q) (car P)))
                  (let ((x (xgcd Z denom n)))
                    (if (> (xgcd->d x) 1)
                      (if (= (xgcd->d x) n)
                        '()
                        (list (xgcd->d x)))
                      (modulo (* (xgcd->u x) (- (cdr Q) (cdr P))) n))))))))
        (let ((l (compute-lambda)))
          (if (list? l)
            l
            (let ((x3 (modulo (- (modexpt l 2 n) (car P) (car Q)) n)))
              (list
                #f
                (cons
                  x3
                  (modulo (- (* l (- (car P) x3)) (cdr P)) n)))))))
      (let retry ()
        (let* ((A (+ 1 (random (- n 2))))
               (a (+ 1 (random (- n 2))))
               (b (+ 1 (random (- n 2))))
               (P0 (cons a b))
               (B (modulo (- (modexpt b 2 n) (modexpt a 3 n) (* A a)) n))
               (delta (+ (* 4 (expt A 3)) (* 27 (expt B 2))))
               (z (gcd delta n)))
          (if (and (not (= z 1)) (not (= z n)))
            (cons z (quotient n z))
            (let try ((l 30) (Pk P0) (Pj P0))
              (if (zero? l)
                (retry)
                (let ((new-k (try-add Pk P0 n A)))
                  (if (null? new-k)
                    (retry)
                    (if (car new-k)
                      (cons (car new-k) (quotient n (car new-k)))
                      (let ((new-j (try-add Pj (cadr new-k) n A)))
                        (if (null? new-j)
                          (retry)
                          (if (car new-j)
                            (cons (car new-j) (quotient n (car new-j)))
                            (try (- l 1) (cadr new-k) (cadr new-j))))))))))))))




    ;; End of module
    ))
