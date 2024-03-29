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

(define-library (alchemy language)
  (export 
    trace
    trace-with
    mtrace
    define-curried
    compose
    applify
    and*
    ; W

    ; export stuff from srfi-1
    iota
    last
    fold
    unfold
    filter
    partition
    remove
    take
    drop
    take-while
    drop-while
    find
    any
    every
    ; member
    ; assoc
    alist-cons
    zip

    string-join

    )
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 1)
          (srfi 232)
          (srfi 152)
          )

  (begin

    (define-syntax mtrace
      (syntax-rules ()
        ((mtrace x)
          (begin 
            (display "Trace: ") (write 'x) (newline)
            x))))

    (define-syntax trace
      (syntax-rules ()
        ((trace x)
          (begin 
            (display "Trace: ") (write x) (newline)
            x))
        ((trace m x)
          (begin
            (display "Trace: ") (display m) (display "\n\t") (write x) (newline)
            x))))

    (define-syntax trace-with
      (syntax-rules ()
        ((_ fn x)
          (begin 
            (display "Trace: \n") (display (fn x)) (newline)
            x))
        ((_ m fn x)
          (begin 
            (display "Trace: \n") (display m) (display "\n\t") (display (fn x)) (newline)
            x))))


    ;;;; Backus, Curry et al.

    ;; Scheme has already the primitive functions to work but lacks of flexible forms.
    ;; We introduce here functional forms as macros for our programs.

    (define applify
      (lambda args
        (lambda x
          (apply apply (append args x)))))

    ;; 1. composition
    ; (f o g):x = f:(g:x)
    (define (compose . fns)
      (define (binary-composition f g)
        (lambda args
          (call-with-values
            (lambda () (apply g args))
            f)))
      (define identity (lambda x (apply values x)))
      (fold-right binary-composition identity fns))

    ;; 2. construction
    ; [f1, ..., fn]:x = <f1:x, ..., fn:x>
    ; Since <..., _|_, ...> = _|_ and all functions are _|_-preserving, so is [f1, ..., fn]
    ; To me this looks like a contra-apply-to-all

    ;; 3. condition
    ; We already have if and it works fine

    ;; 4. constant
    ; k, bye

    ;; 5. insert - aka fold
    ; shall we just use fold from srfi-1 or ?
    ; (define-curried (a:fold fn init ls) (fold fn init ls))

    ;; 6. apply to all - aka better map
    ; ((compose (applify map fn) ...) ls)
    ; (define-curried (a:map fn ls) (map fn ls))
    ; (define-curried (a:map fn . ls) (apply map fn ls))

    ;; 7. binary to unary - aka currying
    ; see define-curried

    ;; 8. while
    ; already have


    ; (define-syntax W
    ;   (syntax-rules ()
    ;     ((_ fn arg)
    ;      (fn arg arg))))

    (define (and* . xz)
      (every (lambda (x) x) xz))

    ))
