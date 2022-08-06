;;;; Cauldrun is where potions are mixed
;; This is a test library to safely test modules.

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
                    (if (eq? e1 e2)
                      (display "\n")
                      (display (format " \tFALSE. ~s != ~s \n" e1 e2))))))))))))))
    
