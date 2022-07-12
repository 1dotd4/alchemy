(cond-expand
  (r7rs)
  (chicken (import r7rs)))

;; Implementation of algorithms found in Chapter 1.
;; csc -X r7rs -R r7rs -sJ -o cohen.fundamental.so cohen.fundemental.scm
(define-library
  (cohen fundamental)
  (import (scheme base))
  (export baz)
  (begin
    (define baz 1)))
