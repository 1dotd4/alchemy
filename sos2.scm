; Scheme Object System
; by d4

; sketches for sybolic data

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

;; sicp §2.4.2 tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE TAG" datum)))
(define (type-tag datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- TYPE TAG" datum)))

;; § 2.4.3 data-directed programming
; table management
; (put <op> <type> <item>)
; (get <op> <type>)

; dispatching on type
; (define (install-rectangular-package)
;   ;; internal procedures
;   (define (real-part z) (car z))
;   (define (imag-part z) (cdr z))
;   (define (make-from-real-imag x y) (cons x y))
;   (define (magnitude z)
;     (sqrt (+ (square (real-part z))
;              (square (imag-part z)))))
;   (define (angle z)
;     (atan (imag-part z) (real-part z)))
;   (define (make-from-mag-ang r a)
;     (cons (* r (cos a)) (* r (sin a))))
;   
;   ;; interface for the rest of the system
;   (define (tag x) (attach-tag 'rectangular x))
;   (put 'real-part '(rectangular) real-part)
;   (put 'imag-part '(rectangular) imag-part)
;   (put 'magnitude '(rectangular) magnitude)
;   (put 'angle '(rectangular) angle)
;   (put 'make-from-real-imag 'rectangular
;        (lambda (x y) (tag (make-from-real-imag x y))))
;   (put 'make-from-mag-ang 'rectangular
;        (lambda (r a) (tag (make-from-mag-ang r a))))
;   'done)
; 
; (define (install-polar-package)
;   ;; internal procedures
;   (define (magnitude z) (car z))
;   (define (angle z) (cdr z))
;   (define (make-from-mag-ang r a) (cons r a))
;   (define (real-part z)
;     (* (magnitude z) (cos (angle z))))
;   (define (imag-part z)
;     (* (magnitude z) (cos (angle z))))
;   (define (make-from-real-imag x y)
;     (cons (sqrt (+ (square x) (square y)))
;           (atan y x)))
;   
;   ;; interface for the rest of the system
;   (define (tag x) (attach-tag 'rectangular x))
;   (put 'real-part '(polar) real-part)
;   (put 'imag-part '(polar) imag-part)
;   (put 'magnitude '(polar) magnitude)
;   (put 'angle '(polar) angle)
;   (put 'make-from-real-imag 'polar
;        (lambda (x y) (tag (make-from-real-imag x y))))
;   (put 'make-from-mag-ang 'polar
;        (lambda (r a) (tag (make-from-mag-ang r a))))
;   'done)
; 
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;             "No method for these types -- APPLY-GENERIC"
;             (list op type-tags))))))
; 
; (define (real-part z) (apply-generic 'real-part z))
; (define (imag-part z) (apply-generic 'imag-part z))
; (define (magnitude z) (apply-generic 'magnitude z))
; (define (angle z) (apply-generic 'angle z))
; (define (make-from-real-imag x y)
;   ((get 'make-from-real-imag 'rectangular) x y))
; (define (make-from-mag-ang r a)
;   ((get 'make-from-mag-ang 'polar) r a))

; now he converts this "intelligent operations" that
; dispatch on data types to "intelligent data objects" that
; dispatch on operation names.

; message passing
; (define (make-from-real-imag x y)
;   (define (dispatch op)
;     (cond ((eq? op 'real-part) x)
;           ((eq? op 'imag-part) y)
;           ((eq? 'magnitude)
;             (sqrt (+ (square x)
;                       (square y))))
;           ((eq? 'angle)
;             (atan x y))
;           (else
;             (error "Unknown op -- MAKE-FROM-REAL-IMAG"
;             op))))
;   dispatch)
; 
; (define (apply-generic op arg) (arg op))

; generic ?
; (define (add x y) (apply-generic 'add x y))
; (define (sub x y) (apply-generic 'sub x y))
; (define (mul x y) (apply-generic 'mul x y))
; (define (div x y) (apply-generic 'div x y))
; 
; (define (install-scheme-number-package)
;   (define (tag x)
;     (attach-tag 'scheme-number x))
;   (put 'add '(scheme-number scheme-number)
;     (lambda (x y) (tag (+ x y))))
;   ...
;   'done)

; add coercion because routing each possibility from the
; table is dumb.
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;         (apply proc (map contents args))
;         (if (= (length args) 2)
;             (let ((type1 (car type-tags))
;                   (type2 (cadr type-tags))
;                   (a1 (car args))
;                   (a2 (cadr args))
;               (let ((t1->t2 (get-coercion type1 type2))
;                     (t2->t1 (get-coercion type2 type1)))
;                 (cond (t1->t2
;                         (apply-generic op (t1->t2 a1) a2))
;                       (t2->t1
;                         (apply-generic op a1 (t2->t1 a2)))
;                       (else
;                         (error "No method for these types"
;                           (list op type-tags)))))))
;             (error "No method for these types"
;               (list op type-tags)))))))

; §2.5.3 Symbolic algebra
; polynomials

;(define (install-polynomial-package)
;  ;; internal procedures
;  ;; representation of poly
;  (define (make-poly variable term-list)
;    (cons variable term-list))
;  (define (variable p) (car p))
;  (define (term-list p) (cdr p))
;
;  ;; representation of terms and term lists
;  ;; add-terms
;  ;; mul-terms
;  ;; mul-term-by-all-terms
;  ;; adjoin-term
;  ;; ...
;  (define (add-poly p1 p2)
;    (if (same-variable? (variable p1) (variable p2))
;        (make-poly (variable p1)
;                  (add-terms (term-list p1)
;                              (term-list p2)))
;        (error "Polys not in same var -- ADD-POLY"
;              (list p1 p2))))
;  (define (add-poly p1 p2)
;    (if (same-variable? (variable p1) (variable p2))
;        (make-poly (variable p1)
;                  (MUL-terms (term-list p1)
;                              (term-list p2)))
;        (error "Polys not in same var -- MUL-POLY"
;              (list p1 p2))))
;
;  ;; interface to rest of the system
;  (define (tag p) (attach-tag 'polynomial p))
;  (put 'add '(polynomial polynomial)
;      (lambda (p1 p2) (tag (add-poly p1 p2))))
;  (put 'mul '(polynomial polynomial)
;      (lambda (p1 p2) (tag (mul-poly p1 p2))))
;  (put 'make 'polynomial
;      (lambda (var terms) (tag (make-poly var terms))))
;  'done)

; implement GCD

;; Rationale
; We wish to impose a type over some primitives and to use
; those types to generalize some procedures. An example that
; will be implemented are Rings that can be composed by
; numbers, polynomials, matrices or more. Thus a type 'ring
; shall be defined by its operations, the interface.
; Following sicp and sdff there's enough material to then
; generalize basic arithmetic operations for all the
; possible combinations of types.


