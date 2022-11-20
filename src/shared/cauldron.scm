;;;; Cauldrun is where potions are mixed
;; This is a test library to safely test modules.

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

; (test description something something-else)
; => Write to terminal current line
; => Write the error if any, otherwise just continue

; https://nitter.lain.la/gynvael/status/1480136346471645184#m
; [hmm]
; [btw]
; [wrr]
; [GRR]

; Chicken note:
; You can use (simple-exceptions) to display the exception
;  (display (format " \tEXCEPTION: ~s in `~s` with ~s \n"
;                   (message e) (location e) (arguments e)))

(define-library (alchemy cauldron)
  (export steer steer-taste steer-observe)
  (import (scheme base)
          (srfi 28))
  (begin

    ; Just try moving
    (define-syntax steer
      (syntax-rules ()
        ((_ description something)
        (call/cc 
          (lambda (k)
            (with-exception-handler
              (lambda (e)
                (begin
                  (display (format " \tEXCEPTION: ~s\n" e))
                  (k 'exception)))
              (lambda ()
                (begin
                  (display description)
                  (let ((e1 something))
                      (display "\n"))))))))))

    ; Try and check (if something ok false)
    (define-syntax steer-taste
      (syntax-rules ()
        ((_ description something)
        (call/cc 
          (lambda (k)
            (with-exception-handler
              (lambda (e)
                (begin
                  (display (format " \tEXCEPTION: ~s\n" e));)
                  (k 'exception)))
              (lambda ()
                (begin
                  (display description)
                  (let ((e1 ((lambda () something))))
                    (if e1
                      (display "\n")
                      (display (format " \tFALSE. ~s \n" e1))))))))))))

    ; Try two and check if equals
    (define-syntax steer-observe
      (syntax-rules ()
        ((_ description something something-else)
        (call/cc 
          (lambda (k)
            (with-exception-handler
              (lambda (e)
                (begin
                  (display (format " \tEXCEPTION: ~s\n" e));)
                  (k 'exception)))
              (lambda ()
                (begin
                  (display description)
                  (let ((e1 ((lambda () something)))
                        (e2 ((lambda () something-else))))
                    (if (equal? e1 e2)
                      (display "\n")
                      (display (format " \tFALSE. ~s != ~s \n" e1 e2))))))))))))))
    
