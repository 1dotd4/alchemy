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

(define-library (alchemy polynomials)
  (export 
    
    make-poly
    ; get-coeffs
    ; get-ring
    poly->string
    poly-degree
    poly+
    poly-
    poly-by-scalar
    poly*

    evaluate-polynomial

    poly-euclidean-division
    poly-gcd
    make-polynomial-ring-over-field

    lagrange-interpolation
    )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy language)
          (alchemy algebra)
          (srfi 1)
          (srfi 27))
  (begin


    ;; TODO: structure for polynomials:
    ;; dense representation
    ;; sparse representation

    (define (remove-leading-zeros coeffs)
      (let ((maybe-new-coeffs (reverse (drop-while zero? (reverse coeffs)))))
        (if (null? maybe-new-coeffs)
          '(0)
          maybe-new-coeffs)))

    (define-record-type <polynomial>
      (make-poly ring coeffs)
      polynomial?
      (ring get-ring set-ring!)
      (coeffs get-coeffs set-coeffs!))

    (define (poly-degree f)
      (if (and (zero? (car (get-coeffs f)))
               (= 1 (length (get-coeffs f))))
        -1
        (- (length (get-coeffs f)) 1)))

    ; (set! (record-printer <polynomial>)
    (define (poly->string f)
    ;   ;;(lambda (record port)
    ;   ;; TODO clean this
    ;   ; (display
      (string-append
        "Polynomial: "
        (if (< (poly-degree f) 0)
          "0"
          (apply
            string-append
            (append
              (map 
                (lambda (z)
                  (string-append
                    (if (not (or (zero? (cadr z))
                                 (= 1 (cadr z))))
                      (number->string (cadr z))
                      "")
                    "x"
                    (if (> (car z) 1)
                      (string-append
                        "^"
                        (number->string (car z)))
                      "")
                    " + "))
                (zip
                  (reverse (iota (poly-degree f) 1 1))
                  (reverse (drop (get-coeffs f) 1))))
              (list (number->string (car (get-coeffs f)))))))
        "\n")
    ;   ; port)))
       )
    ; (define (poly->string f)
    ;   (string-append
    ;     "Polynomial: "
    ;     (apply
    ;       string-append
    ;       (concatenate
    ;       (map (lambda (x) (list " " x))
    ;         (map number->string (get-coeffs f)))))
    ;     "\n"))

    (define (leading-coefficient f)
      (last (get-coeffs f)))

    ; Horner's method
    (define (evaluate-polynomial f x)
      (let ((ring (get-ring f)))
        (let rec ((res (r:zero ring)) (remaining (reverse (get-coeffs f))))
          (if (null? remaining)
            res
            (rec (r:add
                   ring
                   (car remaining)
                   (r:multiply ring x res))
                (cdr remaining))))))

    ;; Polynomial basic operations:
    ;; - addition
    ;; - subtraction
    ;; - scalar multiplication

    (define (coeffs-fix-length coeffs degree)
      (append coeffs (make-list (- (+ 1 degree) (length coeffs)) 0)))

    (define (poly+ f . gs)
      (define (internal-add A B)
        (if (not (eq? (get-ring A) (get-ring B)))
          (error "Cannot perform polynomial addition on different rings (yet)"))
        (let ((R (get-ring A))
              (degree (max (poly-degree A) (poly-degree B))))
          (make-poly
            R
            (remove-leading-zeros
              (map r:add
                   (make-list (+ 1 degree) R)
                   (coeffs-fix-length (get-coeffs A) degree)
                   (coeffs-fix-length (get-coeffs B) degree))))))
      (fold internal-add f gs))

    (define (poly-negate f)
      (let ((R (get-ring f))
            (degree (poly-degree f)))
        (make-poly
          R
          (map r:negate 
               (make-list (+ 1 degree) R)
               (get-coeffs f)))))

    (define (poly- f . gs)
      (apply poly+ (cons f (map poly-negate gs))))

    (define (poly-by-scalar f k)
      (let ((R (get-ring f))
            (degree (poly-degree f)))
        (make-poly
          R
          (map r:multiply
               (make-list (+ 1 degree) R)
               (make-list (+ 1 degree) k)
               (get-coeffs f)))))

    ; Polynomial multiplication
    (define (poly* f g)
      (if (not (eq? (get-ring f) (get-ring g)))
        (error "Cannot perform polynomial multiplication on different rings (yet)"))
      (let ((R (get-ring f))
            (d (+ 1 1 (poly-degree f) (poly-degree g))))
        (make-poly
          R
          (remove-leading-zeros
            (map
              (lambda (k)
                ((compose
                   (applify +)
                   (applify map *))
                 (list
                   (take (coeffs-fix-length (get-coeffs f) k) k)
                   (reverse (take (coeffs-fix-length (get-coeffs g) k) k)))))
              (range 1 d))))))

    ; (apply 
    ;   map
    ;   (lambda (i x)
    ;     (apply
    ;       +
    ;       (apply map *
    ;              (make-list (- (+ 1 k) i) x)
    ;              (get-coeffs g))))
    ;   (zip
    ;     (iota k)
    ;     (get-coeffs f))))))


    ; 3.1.1
    (define (poly-euclidean-division A B)
      (if (not (eq? (get-ring A) (get-ring B)))
        (error "Cannot perform polynomial division on different rings (yet)"))
      ; note A, B \in K[x] where K a field or division make sense
      (let ((field (get-ring A)))
        (let rec ((Q (make-poly field '(0)))
                  (R A))
          (cond
            [(< (poly-degree R) (poly-degree B))
             (list Q R)]
            [else
              (let ((S (make-poly
                         field
                         (append
                           (make-list (- (poly-degree R) (poly-degree B)) 0)
                           (list (r:multiply
                                   field (leading-coefficient R)
                                   (f:inverse field (leading-coefficient B))))))))
                (rec (poly+ Q S)
                     ; note not simply poly* but a scalar and then shift
                     ; try to optimize.
                     (poly- R (poly* S B))))]))))

    (define (make-polynomial-ring-over-field K)
      (make-ring
        polynomial? 'inf
        (make-poly K '(0)) poly+ poly-negate
        (make-poly K '(1)) poly*
        (lambda (A B) (car (poly-euclidean-division A B)))
        (lambda (A B) (< (poly-degree A) (poly-degree B)))))

    (define (poly->monic-polynomial f)
      (let* ((field (get-ring f))
             (k (f:inverse field (leading-coefficient f))))
        (make-poly
          field
          (map
            (lambda (a) (r:multiply field a k))
            (get-coeffs f)))))

    (define (poly-gcd A B)
      (if (not (eq? (get-ring A) (get-ring B)))
        (error "Cannot perform polynomial division on different rings (yet)"))
      (let ((ring (get-ring A)))
        (poly->monic-polynomial
          (gcd (make-polynomial-ring-over-field ring) A B))))

    ;; Discriminant
    ;; Resultant

    ;; Other stuff I think goes here.
    (define (lagrange-interpolation pairs x)
      (define n (length pairs))
      ;
      (define (interpolate pairs i x)
        (let rec ((j 0)
                  (term (cdr (list-ref pairs i))))
          (cond
            ((= j n)
            term)
            ((= j i)
            (rec (+ j 1) term))
            (else
              (rec
                (+ j 1)
                (* term
                  (/ (- x (car (list-ref pairs j)))
                      (- (car (list-ref pairs i))
                        (car (list-ref pairs j))))))))))
      ;
      (let rec ((res 0) (i 0))
        (if (= i n)
          res
          (rec
            (+ res (interpolate pairs i x))
            (+ i 1)))))


    ;; End of module
    ))

