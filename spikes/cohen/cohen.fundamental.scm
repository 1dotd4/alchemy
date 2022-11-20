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

(cond-expand
  (r7rs)
  (chicken (import r7rs)))

;;;; Implementation of algorithms found in Chapter 1.

;; csc -X r7rs -R r7rs -sJ -o cohen.fundamental.so cohen.fundemental.scm

;;; Notations from the book:
;; Algorithm cost in _bit operations_:
;; O(ln(N)):                    linear
;; O(ln^2(N)):                  quadratic
;; O(P(ln(N))):                 polynomial time where P is a polynomial
;; O(N^⍺):                      exponential
;; O(e^{C*sqrt(ln N lnln N)}):  sub-exponential example

;;; Requirements if we didn't have those
;; c = add(a,b)       a+b           = overflow * M + c
;; c = addx(a,b)      a+b+overflow  = overflow * M + c
;; c = sub(a,b)       a-b           = c - overflow * M
;; c = subx(a,b)      a-b-overflow  = c - overflow * M
;; c = mul(a,b)       a*b           = remainder * M + c
;; c = div(a,b)       remainder*M+a = b * c + remainder 
;; c = shiftl(a,k)    2^k * a       = remainder * M + c
;; c = shiftr(a,k)    a*M/2^k       = c*M + remainder
;; c = bfffo(a)       M/2 <= 2^k * a < M (ie. sup(log(M/(2a))) when a != 0 or k = m when a == 0)
;; XXX: Maybe write test for those?
(define (bfffo a)
  (inexact->exact (floor (log 7 2))))

;;; Base Fields and Rings
;; Z/NZ     O(1) because the number of bits is fixed
;; Z        O(ln^2 N)
;; Q        heh
;; R or C   no representation yet but w/e

(define-library
  (cohen fundamental)
  (import (scheme base))
  (export baz)
  (begin
    (define baz 1)

    ;;; The Powering Algorithms
    ;; (G, *) a group.
    ;; recall group are closed under * and every element has an inverse.

    ;; 1.2.1
    (define (right-left-binary g n)
      ;; g \in G, n \in Z
      ;; 1 is the unit
      (if (zero? n) 1
        (if (< n 0)
          (right-left-binary (inverse g) (- n))
          (let rec ((y 1) (N n) (z g))
            (if (zero? N) y
              (if (odd? N)
                (rec (* z y) (quotient N 2) (* z z))
                (rec y       (quotient N 2) (* z z))))))))

    ;; 1.2.2
    (define (left-right-binary g n)
      (if (zero? n) 1
        (if (< n 0)
          (left-right-binary (inverse g) (- n))
          (let ((e (expt 2 (bfffo n))))
            (let rec ((y g) (N (- n E)) (z g) (E e))
              (if (= E 1) y
                (let ((E2 (quotient E 2)))
                  (if (>= N E2)
                    (rec (* y y z) (- N E2) z E2)
                    (rec (* y y)   N        z E2)))))))))

    ;; 1.2.3
    (define (left-right-binary-bits g n)
      (if (zero? n) 1
        (if (< n 0)
          (left-right-binary-bits (inverse g) (- n))
          (let ((e (bfffo n)))
            (let rec ((y g) (N n) (z g) (f e))
              (if (= f 0) y
                (let ((f-1 (sub1 f)))
                  (if (= 1 (modulo (quotient N (expt 2 f-1)) 2))
                    (rec (* y y z) N z f-1)
                    (rec (* y y)   N z f-1)))))))))

    ;; 1.2.4
    (define (left-right-base-2-k g n)
      ;; heh
      (error "Will not found"))

    ;;;Euclid's and Lehmer's Algorithms

    ;; 1.3.1
    (define (gcd-euclit a b)
      (if (zero? b) a
        (euclid-gcd b (modulo a b))))

    ;; 1.3.3
    (define (gcd-lehmer a b)
      ;; todo assert b non--negative and a >= b
      (error "What is multi-precision?"))

    ;; 1.3.5
    ; (define (gcd-binary a b)
    ;   (if (< a b) (gcd-binary b a)
    ;     (if (zero? b) a
    ;       (let rec ((a* a) (b* (modulo a b)))
    ;         (if (zero? b) a
    ;           k = 0
    ;           while a b even
    ;           k += 1
    ;           a /= 2
    ;           b /= 2
    ;           wtf?

    ;; Euclid's Extended Algorithms

    ;; 1.3.6
    (define (xgcd-euclid a b) ; => (u, v, d)
      (if (zero? b) (list 1 0 a)
        (let rec ((u 1) (d a) (v1 0) (v3 b))
          (if (zero? v3)
            (list u (quotient (- d (* a u)) b) d)
            (let* ((q  (quotient d v3))
                   (t3 (modulo   d v3))
                   (t1 (- u (* q v1))))
              (rec v1 v3 t1 t3))))))

    ;; 1.3.7
    (define (xgcd-lehmer a b)
      (error "kek no stop"))

    ;; 1.3.8
    (define (xgcd-binary a b)
      (error "This train doesn't exists"))


    ;; The Chinese Remainder Theorem

    ;; 1.3.11
    ; (define (ctr-steps xmis) ; (... (xi . mi) ...) => x ≡ xi mod mi for all i
    ;   (let rec ((j 2) (C '(1)) (sxmis (sort xmis (lambda (xmi xmj) (< (cdr xmi) (cdr xmj))))))
    ;     (let ((p (modulo (apply * (map cdr (?? sxmis)) mj)

    ;; 1.3.12
    ; note, gcd(mi, mj) = 1 for any pair.
    (define (ctr-inductive xmis) ; (... (xi . mi) ...) => x ≡ xi mod mi for all i
      (let rec ((m (caar xmis)) (x (cdar xmis)) (rxmis (cdr xmis)))
        (if (null? rxmis) x
          (let* ((xmi (car rxmis))
                 (xi (car xmi))
                 (mi (cdr xmi))
                 (r (xgcd-euclid m mi))
                 (u (car r))
                 (v (cadr r))
                 (d (caddr r)))
            (rec (* m mi) (modulo (+ (* u m xi) (* v mi x)) (* m mi)) (cdr xmis))))))

    ;;; Coninued Fraction Expression of Real Numbers
    ;; x = a0 + (1 / (a1 + (1 / a2 + ...)))
    ;; x = [a0, a1, a2, ...]

    ;; 1.3.13
    (define (continue-fraction-lehmer x a a1) ; a < x < a1
      ;; check a and a1 are exact as should be rational!
      (error "okay no, write it when you need it"))

    ; 1.3.14
    (define (continue-fraction-gauss a b) ; a, b independent vectors in a Eucliedean vector space
      (error "okay no, write it when you need it"))

    ;; The Legebdre Symbol

    ; in (Z/nZ)^*
    
    ;; 1.4.3
    (define (order-of-element h g)
      ; G a group
      ; h cardinality of the group
      ; g an element of G
      ; 1 unit element of G
      (let ((fs (factor h)))
        (let rec ((e h) (rfs fs))
          (if (null? rfs) e
            (let ((ee (/ e (expt (caar rfs) (cdar rfs))))
                  (g1 (expt g ee)))
              (let rec2 ((gg1 g1) (eee ee))
                (if (= 1 gg1)
                  (rec ee (cdr rfs))
                  (rec2 (expt g1 (caar rfs))
                        (* ee (caar rfs))))))))))

    ;; 1.4.4
    (define (primitive-root p)
      (error "Will not found"))

    ;; 1.4.10
    (define (kronecker a b)
      (error "Not today please"))


    ;; The Algorithm of Tonelly and Shanks

    ;; 1.5.1
    (define (square-root-mod-p a p)
      (error "copy me from somewhere else"))
    
    ; Okay I'll stop here, this is hurting
    ; I need groups somewhere and ring somewhere else
    ; And maybe fields??
    ; This is because the OOP way is to have a class dispatch operation on datum
    ; But I want the functional(Is it functional?) way to composing operations on objects
    ; Maybe a macro that derive all the funcion from a name and its properties









))
