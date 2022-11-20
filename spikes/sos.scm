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

; Scheme Object System

;;; Tagging system
(define (attach-tag type-tag contents)
        (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE TAG" datum)))
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- TYPE TAG" datum)))

(define (put a b)
  (print "I have stuff: " a " " b " " (symbol? a)))


; (define-syntax bundle
;   (syntax-rules (tag)
;     ((_ type body ...)
;      (eval '(let ((t (lambda (x) (attach-tag type x))))
;        ; ew
;               ((lambda (tag) (begin body ...))
;                t))))))

; (define-syntax bundle
;   (syntax-rules (tag)
;     ((_ type (tag thing))
;      (attach-tag 'type thing))
;     ((_ type (form forms ...))
;      (form (bundle type forms ...)))
;     ((_ type form)
;      form)
;     ((_ type form forms ...)
;      (begin (bundle type form)
;             (bundle type forms ...)))))

; (define-syntax bundle
;   (syntax-rules (tag define quote) ; do I really need to have this huge "special cases" list
;     ((_ type (tag object))
;      (attach-tag 'type object))
;     ((_ type (quote something))
;      (quote something))
;     ((_ type (define head body body* ...))
;      (define head (bundle type body body* ...)))
;     ((_ type (proc proc* ...))
;       (proc (bundle type proc*) ...))
;     ((_ type proc)
;      proc)
;     ((_ type proc proc* ...)
;      (begin (bundle type proc)
;             (bundle type proc* ...)))))

(define-syntax bundle
  (er-macro-transformer
    (lambda (expr rename compare)
      (let ((type (cadr expr))
            (pred (caddr expr))
            (body (cdddr expr)))
        `(let ((tag (lambda (thing)
                      (attach-tag ',type thing))))
           (define (,pred thing) (eq? ',type (type-tag thing)))
           ,@body)))))


;; Example:
(bundle hello hello?
        (print "hello")
        (print (tag 'world)))
 
(bundle
  world world?
  (print (list (tag "hello")
               (tag "hello")
               (list (tag "hello")
                     (tag "hello")))))

(bundle baz baz?
        (define (foo) (tag 'bar))
        (print (+ 2 4 5 6 7 8))
        (put 'foof foo))

(bundle hola hola?
         (print "hello")
         (print (tag "world"))
         (define (foo) 'bar)
         (define bar  (tag 'bar))
         (print (hola? bar))
         (put 'bar-method foo)
         'done)

(print (expand '(bundle foo foo? 'baz)))
(print (expand '(bundle foo foo? '())))
(print (expand '(bundle foo foo? '(1 2 3))))

;;; Message passing
;; This is useful for "objects".
;; Pass symbol to get data, not "methods".
;(define (make-something . args)
;  (define (dispatch op) ;
;    (cond ((eq? op 'attr1) (car args))
;          ((eq? op 'attr2) (cadr args))
;          ;...
;          (else
;            (error "Unknown op -- MAKE-SOMETHING" op))))
;  dispatch);
;; Usage:
; (define X (make-something 'data))
; (X 'attr1)
; (define (apply-generic op arg) (arg op))
; (define (attr1 x) (apply-generic 'attr1 x))
; (attr1 X)
;; Notes: no types

;;; Data-directed
;; This is useful for "procedures"???
;; Pass symbol to get function, not data.
; (define (install-x-package)
;   ;; internal proc
;   (define (attr1 o) (car o))
;   (define (attr2 o) (cadr o))
;   (define (make-x x) x)
;   ;...
;   (define (tag x) (attach-tag 'x x))
;   (put 'meth1 '(required-types ...)
;         (lambda (x y z) (* x y z))) 
;   (put 'make 'x
;       (lambda (x) (tag (make-x x))))
;   (put-coercion 'type-a 'type-b type-a->type-b)
;   'done)
;; Usage:
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;         (apply proc (map contents args))
;         (if (= (length args) 2)
;           ...
;           )))))
; (define (meth1 x) (apply-generic 'meth1 x))
;; Notes: ew a lot of code

;; Conclusion
;; There can be a bundle system and a type system that exploit the bundle system
;; First I need to understand how macro works in details.


;; OOP version: sharing interfaces




;;;
