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

(import 
  (scheme base)
  (scheme write)
  (srfi 28))

(define (mod a n)
  (lambda (op b)
    (case op 
      ((pp)  (display (format "~s mod ~s\n" a n)))
      ((add) (mod (modulo (+ a b) n) n))
      ((sub) (mod (modulo (- a b) n) n))
      ((inv) (mod (modulo (- a) n)   n))
      ((mul) (mod (modulo (* a b) n) n))
      ((pow) (let rec ((x a) (e b) (r 1))
               (if (< e 0) #f
                 (if (> e 0)
                   (if (zero? (modulo e 2))
                     (rec (modulo (* x x) n) (quotient e 2) r)
                     (rec (modulo (* x x) n) (quotient e 2) (modulo (* x r) n)))
                   (mod r n)))))
      (else (error "Operation not found")))))

(let ((a (mod 3 7)))
  ((a 'add 3) 'pp #f)
  ((a 'sub 5) 'pp #f)
  ((a 'mul 3) 'pp #f)
  ((a 'pow 5) 'pp #f))

(define (mod a n) (cons a n))
(define (moduli  a) (car a))
(define (modulus a) (cdr a))
(define (pp a) (display (format "~s mod ~s\n" (moduli a) (modulus a))))
(define (add a b)
  (if (eq? (modulus a) (modulus b))
    (mod (modulo (+ (moduli a) (moduli b)) (modulus a))
         (modulus a))
    (error "Different modulus")))
(define (sub a b)
  (if (eq? (modulus a) (modulus b))
    (mod (modulo (- (moduli a) (moduli b)) (modulus a))
         (modulus a))
    (error "Different modulus")))
(define (mul a b)
  (if (eq? (modulus a) (modulus b))
    (mod (modulo (* (moduli a) (moduli b)) (modulus a))
         (modulus a))
    (error "Different modulus")))
(define (pow a b)
  (let rec ((x (moduli a)) (e b) (r 1))
    (if (< e 0) #f
      (if (> e 0)
        (if (zero? (modulo e 2))
          (rec (modulo (* x x) (modulus a)) (quotient e 2) r)
          (rec (modulo (* x x) (modulus a)) (quotient e 2) (modulo (* x r) (modulus a))))
        (mod r (modulus a))))))

(let ((a (mod 3 7))
      (b (mod 5 7))
      (c (mod 2 11)))
  (pp (add a a))
  (pp (sub a b))
  (pp (mul a a))
  (pp (pow a 5))
  ; (pp (add a c))
  )
