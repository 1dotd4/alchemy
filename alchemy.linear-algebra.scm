(define-library (alchemy linear-algebra)
  (export 
    range
    ma-swap-col!
    ma-pp
    v-pp
    square-linear-system
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

    ;; [i , j)
    ;; 3 4 5 6
    ;; 0 1 2 3
    ;; => 4
    (define (range i j)
      (iota (- j i) i 1))

    (define (v-swap! vec i j)
      (let ((t (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j t)))

    (define (v-pp vec)
      (let ((len (vector-length vec)))
        (display "[ ")
        (map (lambda (i)
               (begin
                 (display (number->string (vector-ref vec i))))
               (if (= len (+ 1 i))
                 (display " ]\n")
                 (display ", ")))
             (iota len))))

    ;; Note that matrix is represented column-wise
    (define (ma matrix row column) (vector-ref (vector-ref matrix column) row))
    (define (ma-set! matrix row column value)
      (vector-set! (vector-ref matrix column) row value)
      matrix)

    (define (ma-swap-col! matrix i j)
      (let ((tmp (vector-copy (vector-ref matrix i))))
        (vector-set! matrix i (vector-ref matrix j))
        (vector-set! matrix j tmp)
        matrix))

    (define (ma-swap! A ai aj bi bj)
      (let ((tmp (ma A ai aj)))
        (ma-set! A ai aj (ma A bi bj))
        (ma-set! A bi bj tmp)
        A))

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

    (define (vector-swap! v i j)
      (let ((tmp (vector-ref v i)))
        (vector-set! v i (vector-ref v j))
        (vector-set! v j tmp)
        v))

;     ;; 2.2.1
;     (define (square-linear-system M B)
;       (let ((n (vector-length A))
;             (C (make-vector n)))
;       ;; M is nxn
; 
;       (define (find-non-zero-i A j)
;         (let rec ((i j))
;           (cond
;             ((= n i) #f)
;             ((zero? (ma A i j)) (rec (+ 1 i)))
;             (else i))))
;       (define (swap-step! A i j)
;         (map
;           (lambda (l)
;             (ma-swap! A i l j l))
;           (range j n)))
;       (define (divide-row! A i j)
;         (let ((C (make-vector n)))
;           (map
;             (lambda (l)
;               )
;             (range j n))))
; 
;       (let rec ((j 0))
;         (if (> j n)
;           (solve-triangular M B)
;           (let ((i (find-non-zero-i M j)))
;             (if (not i)
;               #f
;               (begin
;                 ;; swap if needed
;                 (if (> i j)
;                   (begin
;                     (swap-step! M i j)
;                     (vector-swap! B i j)))
;                 ;; eliminate
;                 (let ((d (/ 1 (ma M j j))))
;                   (map ... fill my C )
;                   (map
;                     (lambda (k)
;                       (map
;                         (lambda (l)
; 
;                           )
;                         (range ?? n)) ;; columns
;                       (vector-set! B k ??))
;                     (range ?? n))) ; rows


    ;; 2.2.1
    (define (square-linear-system M-orig B-orig)
      (define n (vector-length M-orig))
      (define M (vector-copy M-orig))
      (define B (vector-copy B-orig))
      (define C (make-vector n 0))
      (define X (make-vector n 0))
      (let loop ((j 0) (i 0))
        (cond
          ; solve linear system
          [(= j n)
           ; Here M is an upper triangular matrix
           (do ((i (- n 1) (- i 1)))
             ((< i 0) X)
             (vector-set! X i
                          (/ (- (vector-ref B i)
                                (fold
                                  (lambda (j s) (+ s (* (ma M i j) (vector-ref X j))))
                                  0
                                  (range i n)))
                             (ma M i i))))]
          ; 3. all zero entry
          [(= n i)
           (begin
             (ma-pp M)
             (error "Not invertible matrix!"))]
          ; 3. find non-zero entry
          [(zero? (ma M i j)) (loop j (+ 1 i))]
          ; coninue
          (else
            (begin
              ; 4. Swap?
              (if (> i j)
                (begin
                  (display "I need to swap!")
                  (do ((l j (+ l 1)))
                    ((= l n) '())
                      (ma-swap! M i l j l))
                  (v-swap! B i j)))
              ; 5. elminiate from M[j,j]
              ; Get the pivot inverse and apply to current columnt that will be propagated
              (let ((d (/ 1 (ma M j j))))
                (do ((k (+ j 1) (+ k 1)))
                  ((= k n) '())
                  (vector-set! C k (* d (ma M k j)))))
              ; >"Note that we do no need to compute this"
              ; Yeah but if you don't say I have to set it...
              (do ((k (+ j 1) (+ k 1)))
                ((= k n) '())
                (ma-set! M k j 0))
              ; Loop to eliminate on matrix
              (do ((k (+ j 1) (+ k 1)))
                ((= k n) '())
                (do ((l (+ j 1) (+ l 1)))
                  ((= l n) '())
                  (ma-set! M k l (- (ma M k l)
                                    (* (vector-ref C k)
                                       (ma M j l))))))
              ; Loop to propagate on the constants
              (do ((k (+ j 1) (+ k 1)))
                ((= k n) '())
                (vector-set! B k
                             (- (vector-ref B k)
                                (* (vector-ref C k)
                                   (vector-ref B j)))))
              ; continue with the next column
              (loop (+ j 1) (+ j 1)))))))








    ))
