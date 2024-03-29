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

    ;; make-multivariate-poly
    mpoly-zero?
    mpoly->string
    mpoly-<glex
    mpoly-<lex
    mpoly+
    mpoly*
    mleading-monomial
    mleading-coeff
    mpoly-euclidean-division-residue
    s-polynomial
    grobner-basis
    make-multivariate-polynomial-ring-over-field-with-order
    FXs
    )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy language)
          (alchemy algebra)
          (srfi 27)
          (srfi 95)
          (srfi 152))
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
    ;; TODO change poly-type to list

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


    ;; Multivariate polynomials

    ; Lists are good enough for Grobner
    ; Structure of a monomial (1/2 x y^3 z^2) => (1/2 1 3 2)
    ; Structure of a polynomial (y^2 - z) => ((1 0 2 0) (1 0 0 1))
    ; ...but what about variable substitution?

    ;; monomial order
    (define (mpoly-<lex ma mb)
      (if (not (= (length ma) (length mb)))
        (error "monomials of multivariate polynomial not in the same ring.")
        (let rec ((da (cdr ma)) (db (cdr mb)))
          (if (null? ma)
            #f ;; they are equal
            (cond
              [(= (car da) (car db)) (rec (cdr da) (cdr db))] ;; check next
              [else (< (car da) (car db))]))))) ;; there is one bigger than another

    (define (mpoly-<glex ma mb)
      (if (not (= (length ma) (length mb)))
        (error "monomials of multivariate polynomial not in the same ring.")
        (let ((da (fold + 0 (cdr ma)))
              (db (fold + 0 (cdr mb))))
          (cond
            [(= da db) (mpoly-<lex ma mb)] ;; they are the same total degree, check with lex
            [else (< da db)])))) ;; there is one bigger

    (define (mpoly-zero? mp) (null? mp))

    (define (mpoly->string mp)
      ;; TODO clean this
      (string-append
        "Multivariate polynomial: "
        (if (mpoly-zero? mp)
          "0"
          (string-join
            (map
              (lambda (monomial)
                (string-append
                  (if (= 1 (car monomial))
                    ""
                    (number->string (car monomial)))
                  (apply string-append
                         (map
                           (lambda (deg letter)
                             (if (zero? deg)
                               ""
                               (string-append
                                 letter
                                 (if (= 1 deg)
                                   ""
                                   (string-append
                                     "^"
                                     (number->string deg))))))
                           (cdr monomial)
                           (map (compose list->string list) (string->list "xyzt"))))))
              mp)
            " + "))
        "\n"))

    ;; XXX: note this is the inner ring, the one of the coefficients!
    (define (mpoly+ R less? f . gs)
      (define (prune-zero-coeffs m)
        (not (zero? (car m))))
      (define (monomial-addition monomial current-f)
        (let rec ((toview current-f) (done '()))
          (if (null? toview)
            (cons monomial done)
            (let ((current-monomial (car toview)))
              (if (equal? (cdr current-monomial) (cdr monomial))
                (filter
                  prune-zero-coeffs
                  (append
                    (reverse done)
                    (list
                      (cons
                        (r:add R (car monomial) (car current-monomial))
                        (cdr current-monomial)))
                    (cdr toview)))
                (rec (cdr toview) (cons current-monomial done)))))))
      (define (internal-add f g)
        (fold monomial-addition f g))
      (reverse (sort (fold internal-add f gs) less?)))

    (define (mpoly* R less? f . gs)
      ;; TODO: check R is actually a ring...
      (define (internal-addition R)
        (lambda (f g) (mpoly+ R less? f g)))
      (define (monomial-multiplication current-f)
        (lambda (monomial)
          (map
            (lambda (f-monomial)
              (cons
                (r:multiply R (car f-monomial) (car monomial))
                (map + (cdr f-monomial) (cdr monomial))))
            current-f)))
      (define (internal-multiply f g)
        (apply ;; I hate to do this...
          mpoly+ R less?
          (map (monomial-multiplication f) g)))
      (fold internal-multiply f gs))

    (define (mpoly-negate R f)
      (define (monomial-negate m)
        (cons (r:negate R (car m)) (cdr m)))
      (map monomial-negate f))

    (define (mleading-term less? mp)
      (car (reverse (sort mp less?))))

    (define (mleading-monomial less? mp)
      (cons 1 (cdr (mleading-term less? mp))))

    (define (mleading-coeff less? mp)
      (car (mleading-term less? mp)))

    ;;; XXX: the ordering should be inside the ring
    (define (mmonomial-division a b)
      (list
        (cons
          (/ (car a) (car b))
          (map - (cdr a) (cdr b)))))

    (define (mpoly-euclidean-division-residue R less? initial-g fs)
      (define (monomial-divisible? a b)
        (fold and* #t (map <= (cdr a) (cdr b))))
      (let rec ((g initial-g) (r '()))
        (if (mpoly-zero? g)
          (reverse (sort r less?))
          (let try ((rfs fs))
            (if (null? rfs)
              (rec
                (mpoly+ R less? g (mpoly-negate R (list (mleading-term less? g))))
                (mpoly+ R less? r (list (mleading-term less? g))))
              (if (monomial-divisible? (mleading-monomial less? (car rfs))
                                       (mleading-monomial less? g))
                (let* ((gamma (mmonomial-division
                                (mleading-term less? g)
                                (mleading-term less? (car rfs))))
                      (f* (mpoly* R less? (mpoly-negate R gamma) (car rfs))))
                  (rec (mpoly+ R less? g f*) r))
                (try (cdr rfs))))))))

    ;; XXX: todo, this is a higher level function, we shall not have such interface
    (define (s-polynomial R less? f g)
      (define (monomial-lcm a b)
        (cons 1 (map max (cdr a) (cdr b))))
      (let* ((gamma (monomial-lcm
                      (mleading-monomial less? f)
                      (mleading-monomial less? g)))
              (alpha (mmonomial-division gamma (mleading-term less? f)))
              (beta  (mpoly-negate R (mmonomial-division gamma (mleading-term less? g)))))
        (mpoly+
          R
          less?
          (mpoly* R less? alpha f)
          (mpoly* R less? beta g))))


    (define (grobner-basis R less? initial-gs)
      (let rec ((current-base initial-gs))
        (let for-f ((f-to-do initial-gs))
          (if (null? f-to-do)
            current-base
            (let for-g ((g-to-do initial-gs))
              (if (null? g-to-do)
                (for-f (cdr f-to-do))
                (let ((s-bar (mpoly-euclidean-division-residue
                               R less?
                               (s-polynomial R less? (car f-to-do) (car g-to-do))
                               current-base)))
                  (if (mpoly-zero? s-bar)
                    (for-g (cdr g-to-do))
                    (rec (append current-base (list s-bar)))))))))))


    (define (make-multivariate-polynomial-ring-over-field-with-order K less? number-of-variables)
      ;; TODO: how to get inner ring?
      (make-ring
        polynomial? 'inf
        '()
        (lambda (A B) (mpoly+ K less? A B))
        (lambda (A) (mpoly-negate K A))
        (list (cons (r:one K) (make-list number-of-variables (r:zero K))))
        (lambda (A B) (mpoly* K less? A B))
        (lambda (A B) (error "does not make sense to me right now"))
        less?))

    (define FXs make-multivariate-polynomial-ring-over-field-with-order)


    ;;; 3.4 Factorization of Polynomials Modulo p
    ;; (define (berlekamp-factorization p f)
    ;;   ; p an integer that is either a prime or a prime power
    ;;   ; f a polynomial with coefficients modulo p

    ;; Berlekamp-Massey

    ;; End of module
    ))

