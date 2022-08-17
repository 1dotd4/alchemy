(define-library (alchemy linear-algebra)
  (export 
    ma-swap-col!
    ma-pp
    rho
    transpose)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy algebra)
          (srfi 1)
          (srfi 27))
  (begin

    ;; TODO: move this somewhere else
    (define (cartesian-product xs ys)
        (if (or (zero? (length xs)) (zero? (length ys)))
          '()
          (fold append '() (map (lambda (x) (map (lambda (y) (list x y)) ys)) xs))))

    ;; Note that matrix is represented column-wise
    (define (ma matrix row column) (vector-ref (vector-ref matrix column) row))
    (define (ma-set! matrix row column value) (vector-set! (vector-ref matrix column) row value))

    (define (ma-swap-col! matrix i j)
      (let ((tmp (vector-copy (vector-ref matrix i))))
        (vector-set! matrix i (vector-ref matrix j))
        (vector-set! matrix j tmp)
        matrix))

    (define (ma-pp A)
      (let ((columns (vector-length A))
            (rows (vector-length (vector-ref A 0))))
        (display "[ ")
        (map
          (lambda (i)
            (map
              (lambda (j)
                (begin
                  (display (number->string (ma A i j)))
                  (if (= columns (+ 1 j)) 
                    (if (= rows (+ 1 i))
                      (display "]\n")
                      (display ",\n  "))
                    (display ",\t"))))
              (iota columns)))
          (iota rows))))

    ;; This one exists to help you write, not to help you calculate, infact it's row-wise
    (define (rho rows columns data)
      (let ((A (make-vector columns)))
          (map (lambda (j) (vector-set! A j (make-vector rows))) (iota columns))
          (map
            (lambda (x) (ma-set! A (quotient (car x) columns) (modulo (car x) columns) (cadr x)))
            (zip (iota (* rows columns)) data))
          A))

    (define (transpose A)
      (let* ((new-rows (vector-length A))
             (new-columns (vector-length (vector-ref A 0)))
             (B (make-vector new-columns)))
        (map (lambda (j) (vector-set! B j (make-vector new-rows))) (iota new-columns))
        (map
          (lambda (x) (ma-set! B (car x) (cadr x) (ma A (cadr x) (car x))))
          (cartesian-product (iota new-rows) (iota new-columns)))
        B))


    ))
