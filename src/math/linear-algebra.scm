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

(define-library (alchemy linear-algebra)
  (export 
    ma-swap-col!
    matrix->string
    vec->string
    square-linear-system
    rho
    matrix-identity
    matrix-inverse
    matrix-multiplication
    matrix-determinant
    matrix-kernel
    transpose

    v-add
    gram-schmidt
    LLL
    )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (scheme inexact)
          (alchemy language)
          (alchemy algebra)
          (srfi 27))
  (begin

    (define (v-swap! vec i j)
      (let ((t (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j t)))

    (define (vec->string vec)
      (let ((len (vector-length vec)))
        (string-append
          "[ "
          (apply
            string-append
            (map (lambda (i)
                   (string-append
                     (number->string (vector-ref vec i))
                     (if (= len (+ 1 i))
                       " ]\n"
                       ", ")))
                 (iota len))))))

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

    (define (ma-swap-rows! matrix i j)
      (let ((tmp (transpose matrix)))
        (ma-swap-col! tmp i j)
        (set! matrix tmp)
        matrix))


    (define (ma-swap! A ai aj bi bj)
      (let ((tmp (ma A ai aj)))
        (ma-set! A ai aj (ma A bi bj))
        (ma-set! A bi bj tmp)
        A))

    (define (matrix->string A)
      (let ((columns (vector-length A))
            (rows (vector-length (vector-ref A 0))))
        (string-append
          "[ "
          (apply string-append
                 (map
                   (lambda (i)
                     (apply string-append
                            (map
                              (lambda (j)
                                (string-append
                                  (number->string (ma A i j))
                                  (if (= columns (+ 1 j)) 
                                    (if (= rows (+ 1 i))
                                      " ]\n"
                                      ",\n  ")
                                    ",\t")))
                              (iota columns))))
                   (iota rows))))))

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

    (define matrix-zero
      (case-lambda
        ((n) (matrix-zero n n))
        ((n m)
         (let ((I (rho n m (iota (* n m)))))
           (do ((i 0 (+ i 1)))
             ((= i n) I)
             (do ((j 0 (+ j 1)))
               ((= j m) '())
               (ma-set! I i j 0)))))))

    (define (matrix-identity n)
      (let ((I (rho n n (iota (* n n)))))
        (do ((i 0 (+ i 1)))
          ((= i n) I)
          (do ((j 0 (+ j 1)))
            ((= j n) '())
            (ma-set! I i j (if (= i j) 1 0))))))

    (define (ma-col A j) (vector-ref A j))
    (define (ma-row A j) (vector-ref (transpose A) j))

    (define (ma-row-set! A j row)
      (let ((l (vector-length row)))
        (if (not (= l (vector-length A)))
          (error "Different length for matrix and row vector" A row)
          (do ((i 0 (+ i 1)))
            ((= i l) A)
            (ma-set! A j i (vector-ref row i))))))

    (define (scalar-multiplication s v)
      (let* ((lv (vector-length v))
             (vo (make-vector lv)))
        (do ((i 0 (+ i 1)))
          ((= i lv) vo)
          (vector-set! vo i (* (vector-ref v i) s)))))

    (define (v-sum v) (fold + 0 (vector->list v)))

    (define (v-binary binary-operation va vb)
      (let* ((la (vector-length va))
             (lb (vector-length vb))
             (vo (make-vector la)))
        (if (not (= la lb))
          (error "Different lengths" la lb)
          (do ((i 0 (+ i 1)))
            ((= i la) vo)
            (vector-set! vo i (binary-operation
                                (vector-ref va i)
                                (vector-ref vb i)))))))

    (define (inner-product va vb)
      (let ((l (vector-length va)))
        (if (not (= l (vector-length vb)))
          (error "Different lengths inner product" va vb)
          ((compose
             (applify +)
             (applify map *))
           (map vector->list (list va vb))))))
          ; ((compose
          ;    +
          ;    (lambda (a) (apply map * 
          ;    (a:fold + 0)
          ;    (a:map
          ;      (a:compose
          ;        (a:fold * 1)
          ;        vector->list))
          ;    vector->list
          ;    transpose)
          ;  (list va vb)))))
          ; (v-sum (v-binary * va vb)))))

    (define (matrix-multiplication A B)
      (let* ((ra (vector-length (vector-ref A 0)))
             (ca (vector-length A))
             (rb (vector-length (vector-ref B 0)))
             (cb (vector-length B))
             (AB (matrix-zero ra cb)))
        (if (not (= ca rb))
          (error "Can't multiply those matrices" A B)
          (do ((i 0 (+ i 1)))
            ((= i ra) AB)
            (do ((j 0 (+ j 1)))
              ((= j cb) '())
              (ma-set! AB i j (inner-product (ma-row A i) (ma-col B j))))))))


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
           (begin
           (do ((i (- n 1) (- i 1)))
             ((< i 0) X)
             (vector-set! X i
                          (/ (- (vector-ref B i)
                                (fold
                                  (lambda (j s) (+ s (* (ma M i j) (vector-ref X j))))
                                  0
                                  (range i n)))
                             (ma M i i)))))]
          ; 3. all zero entry
          [(= n i)
           (begin
             (write (matrix->string M))
             (error "Not invertible matrix!"))]
          ; 3. find non-zero entry
          [(zero? (ma M i j)) (loop j (+ 1 i))]
          ; coninue
          (else
            (begin
              ; 4. Swap?
              (if (> i j)
                (begin
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


    ; 2.2.2
    (define (matrix-inverse M-orig)
      ;;; TODO: Remark (1) make it obvious. If B is replaced by any other n x m matrix N you get M^-1 N.
      ;;;       this means that with m = 1 you can get to solve 2.2.1
      ;;;       (faster for n > 4 because of n^3 * 3/4 complexity)
      (define n (vector-length M-orig))
      (if (not (= n (vector-length (vector-ref M-orig 0))))
        (error "Not square matrix"))
      (define M (vector-copy M-orig))
      (define C (make-vector n 0))
      (define B (matrix-identity n))
      (define X (matrix-zero n))
      (let loop ((j 0) (i 0))
        (cond
          ; solve triangular system
          [(= j n)
           ; Here M is an upper triangular matrix
           (begin
             (do ((i (- n 1) (- i 1)))
               ((< i 0) X)
               (ma-row-set!
                 X i
                 (scalar-multiplication
                   (/ 1 (ma M i i))
                   (v-binary
                     -
                     (ma-row B i)
                     (fold
                       (lambda (j s)
                         (v-binary
                           +
                           s 
                           (scalar-multiplication
                             (ma M i j)
                             (ma-row X j))))
                       (make-vector n 0)
                       (range (+ 1 i) n)))))))]
          ; 3. all zero entry
          [(= n i)
           (begin
             (write (matrix->string M))
             (error "Not invertible matrix!"))]
          ; 3. find non-zero entry
          [(zero? (ma M i j)) (loop j (+ 1 i))]
          ; coninue
          (else
            (begin
              ; 4. Swap?
              (if (> i j)
                (begin
                  (do ((l j (+ l 1)))
                    ((= l n) '())
                    (ma-swap! M i l j l))
                  (ma-swap-rows! B i j)))
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
              ; Loop to propagate on the constants matrix
              (do ((k (+ j 1) (+ k 1)))
                ((= k n) '())
                (ma-row-set!
                  B
                  k
                  (v-binary
                    -
                    (ma-row B k)
                    (scalar-multiplication
                      (vector-ref C k)
                      (ma-row B j)))))
              ; continue with the next column
              (loop (+ j 1) (+ j 1)))))))

    ; 2.2.3
    ;; using ordinary elimination
    (define (matrix-determinant M-orig)
      ; check square
      (define n (vector-length M-orig))
      (if (not (= n (vector-length (vector-ref M-orig 0))))
        (error "Not square matrix"))
      (define M (vector-copy M-orig))
      (define C (make-vector n 0))
      ;; more swaps..
      (let loop ((j 0) (i 0) (x 1))
        (cond
          ; 2. Finished?
          [(= j n) x]
          ; 3. Found all zero entry
          [(= i n) 0]
          ; 3. Find non-zero entry
          [(zero? (ma M i j)) (loop j (+ 1 i) x)]
          ; 4. swap?
          [(> i j)
                (begin
                  (do ((l j (+ l 1)))
                    ((= l n) '())
                    (ma-swap! M i l j l))
                  (loop j j (- x)))]
          [else
            (begin
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
              ; continue with the next column
              (loop (+ j 1) (+ j 1) (* x (ma M j j))))])))

    ;; 2.2.6 Determinant using Gauss Bareiss on a matrix M with coefficient in an integral domain R.

    ;; 2.2.7 Characteristic Polynomial and Adjoint Matrix

    ;; 2.2.9 Hessenberg

    ;; 2.3.1 Kernel of a Matrix
    (define (matrix-kernel M-orig)
      (define M (vector-copy M-orig))
      (define n (vector-length M))
      (define m (vector-length (vector-ref M 1)))
      (define C (make-vector m 0))
      (define D (make-vector n -1))
      (define X '())
      (let loop ((r 0) (j 0) (k 0))
        ; r counts the number of 0 columns
        ; j and k are resp. row and column
        (cond
          ; 5. Output kernel
          [(= k n)
           ; for every k, 1 <= k <= n and d_0 = 0
           ; (there will be exactly r such k),
           ; output the column vector X = (x_i) 1<=i<=n
           ; defined by
           ; x_i = m_di,k if di > 0,
           ;       1      if i = k,
           ;       0      otherwise
           ; These r vector form a basis for the kernel of M.
           (let rec ((X (make-list r 0)) (k 0) (z 0))
             (cond
               [(= k n) X]
               [(not (= (vector-ref D k) -1))
                (rec X (+ k 1) z)]
               [else
                (begin
                  (set! (list-ref X z) (make-vector n 0))
                  (do ((i 0 (+ i 1)))
                    ((= i n) '())
                    (cond
                      [(>= (vector-ref D i) 0)
                        (vector-set! (list-ref X z) i (ma M (vector-ref D i) k))]
                      [(= i k)
                        (vector-set! (list-ref X z) i 1)]
                      [else
                        (vector-set! (list-ref X z) i 0)]))
                  (rec X (+ k 1) (+ z 1)))]))]
          ; 4. finished?
          [(= j m) (loop (+ r 1) j (+ k 1))]
          ; 2. Scan column
          [(or (zero? (ma M j k))
               (not (zero? (vector-ref C j))))
            (loop r (+ j 1) k)]
                                              
          ; 3. Eliminate
          [else
            (begin
              (let ((d (- (/ 1 (ma M j k)))))
                (ma-set! M j k -1)
                (do ((s (+ k 1) (+ s 1)))
                  ((= s n) '())
                  (ma-set! M j s (* d (ma M j s)))))
              (do ((i 0 (+ i 1)))
                ((= i m) '())
                (if (not (= i j))
                  (let ((d (ma M i k)))
                    (ma-set! M i k 0)
                    (do ((s (+ k 1) (+ s 1)))
                      ((= s n) '())
                      (ma-set! M i s
                               (+ (ma M i s)
                                  (* d (ma M j s))))))))
              (vector-set! C j k)
              (vector-set! D k j)
              (loop r (+ j 1) (+ k 1)))])))
    
    ;; 2.3.2 Image of a Matrix

    ;; ...

    ;; 2.3.4 Inverse Image
    ;; 2.3.5 Inverse Image Matrix

    ;; 2.3.6 Supplement a Basis

    ;; 2.3.8 Sum of Subspaces
    ;; 2.3.8 Intersection of Subspaces

    ;; 2.4.14 Smith Normal Form

    ;;;;;;;;;;;;;;;;;;;;;;; YOU ARE HERE

    (define v-add
      (case-lambda
        ((a) a)
        ((va vb)
          (list->vector
            (map + 
                 (vector->list va)
                 (vector->list vb))))
        ((va . vbs)
         (apply v-add (cons (v-add va (car vbs)) (cdr vbs))))))

    (define (gram-schmidt vectors)
      (define (compute-muij bi)
        (lambda (bj)
          (/ (inner-product bi bj)
             (inner-product bj bj))))
      (let rec ((gram-base (list (car vectors)))
                (mus '())
                (remaining-vectors (cdr vectors)))
        (if (null? remaining-vectors)
          (cons gram-base mus)
          (let* ((bi (car remaining-vectors))
                 (muijs (map (compute-muij bi) gram-base))
                 (-muijs (map - muijs))
                 (bi* (apply v-add (cons bi (map scalar-multiplication -muijs gram-base)))))
            (rec
              ;; (cons bi* gram-base)
              (append gram-base (list bi*))
              (append mus (list muijs))
              (cdr remaining-vectors))))))

    ;; 2.6.3 LLL Algorithm
    (define (eucliedean-norm-squared v)
      (inner-product v v))

    (define (pid a)
      (display a)
      (display "\n")
      a)

    (define (LLL M delta)
      (define starting-rows (vector->list (transpose M)))
      (define n (length starting-rows))
      ;; Internal state: X, X*, M, k
      (define (get-mu M j i)
        (cond
          ((= j i) 1)
          ((< j i) 0)
          (else
            (list-ref (list-ref M (- j 1)) i))))
      (define (check-state state)
        (let* ((k (list-ref state 3))
               (X* (cadr state))
               (M (list-ref state 2))
               (kth (list-ref X* k))
               (kth-1 (list-ref X* (- k 1)))
               (mukk-1 (get-mu M k (- k 1))))
          ; (display "Deciding with following mukk-1: ")
          ; (display mukk-1)
          ; (display "\n")
          (>= (eucliedean-norm-squared kth)
              (* (- delta (square mukk-1))
                 (eucliedean-norm-squared kth-1)))))

      (define (internal-swap state)
        (let* ((vectors (car state))
               (k (list-ref state 3))
               (new-X (append
                        (take vectors (- k 1))
                        (list
                          (list-ref vectors k)
                          (list-ref vectors (- k 1)))
                        (drop vectors (+ k 1))))
               (new-GS (gram-schmidt new-X)))
          ; (display "Exchanged ")
          ; (display k)
          ; (display "\n")
          (list
            new-X
            (car new-GS)
            (cdr new-GS)
            (max 1 (- (list-ref state 3) 1)))))

      (define (internal-reduce state l)
        (let* ((X (car state))
               (k (list-ref state 3))
               (B-GS-stuff (gram-schmidt X))
               (mus (cdr B-GS-stuff))
               (X* (car B-GS-stuff))
               (mukl (get-mu mus k l)))
          ; (trace-with (compose matrix->string list->vector) X*)
          ; (display "Reducing ")
          ; (display k)
          ; (display ", ")
          ; (display l)
          ; (display ": ")
          ; (display mukl)
          ; (display "\n")
          ; (matrix->string (transpose (list->vector (car state))))
          (if (<= (abs mukl) 1/2)
            (list X X* mus k)
            (let* ((big-mu (ceiling (- mukl 1/2)))
                   (new-X (append
                            (take X k)
                            (list
                              (v-add
                                (list-ref X k)
                                (scalar-multiplication
                                  (- big-mu)
                                  (list-ref X l))))
                            (drop X (+ k 1))))
                   (other-GS (gram-schmidt new-X)))
              (list
                new-X
                (car other-GS)
                (cdr other-GS)
                k)))))

      (define (internal-recursive-reduce state)
        (let rec ((l (- (list-ref state 3) 2)) (next-state state))
          (if (< l 0)
            (begin
              ; (display "Ended the clause two\n")
              (list
                (car next-state)
                (cadr next-state)
                (list-ref next-state 2)
                (+ 1 (list-ref next-state 3))))
            (rec (- l 1) (internal-reduce next-state l)))))

      (let main-procedure ((internal-state (list starting-rows '() '() 1)))
        ; (display "Now in main \n")
        ; (matrix->string (transpose (list->vector (car internal-state))))
        (if (= (list-ref internal-state 3) n)
          (transpose (list->vector (car internal-state)))
          (let* ((reduced-state (internal-reduce internal-state (- (list-ref internal-state 3) 1))))
            ; (display "Now before decing\n")
            ; (matrix->string (transpose (list->vector (car reduced-state))))
            (if (check-state reduced-state)
              (main-procedure (internal-recursive-reduce reduced-state))
              (main-procedure (internal-swap reduced-state)))))))



    ;; 2.6.4 LLL Algorithm with Deep Insertions
    ;; 2.6.7 Integral LLL Algorithm
    ;; 2.6.8 LLL Algorithm on Not Necessarily Independant Vectors
    ;; 2.7.1 Kernel and Image of a Matrix Using LLL
    ;; 2.7.2 Kernel over Z Using LLL
    ;; 2.7.4 Linear Dependence
    ;; 2.7.5 Short vectors
    ;; 2.7.6 Cholesky Decomposition
    ;; 2.7.7 Fincke-Pohst








    ))
