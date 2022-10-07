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

    poly-euclidean-division
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
      (reverse (drop-while zero? (reverse coeffs))))

    (define-record-type <polynomial>
      (make-poly ring coeffs)
      polynomial?
      (ring get-ring set-ring!)
      (coeffs get-coeffs set-coeffs!))

    (define (poly-degree f)
      (- (length (get-coeffs f)) 1))

    (define (poly->string f)
      ;; TODO clean this
      (string-append
        "Polynomial: "
        (apply
          string-append
          (append
            (map 
              (lambda (z)
                (string-append
                  (if (> (cadr z) 1)
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
            (list (number->string (car (get-coeffs f))))))
        "\n"))

    (define (leading-coefficient f)
      (last (get-coeffs f)))

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
      (let ((ring (get-ring A)))
        (let rec ((Q (make-poly ring '(0)))
                  (R A))
          (cond
            [(< (poly-degree R) (poly-degree B))
             (list Q R)]
            [else
              (let ((S (make-poly
                         ring
                         (append
                           (make-list (- (poly-degree R) (poly-degree B)) 0)
                           (list (/ (leading-coefficient R)
                                    (leading-coefficient B)))))))
                (rec (poly+ Q S)
                     ; note not simply poly* but a scalar and then shift
                     ; try to optimize.
                     (poly- R (poly* S B))))]))))




    ;; End of module
    ))
