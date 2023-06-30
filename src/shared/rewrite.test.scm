(define test-vectors
  (list
    (make-sequent '())
    (make-sequent '() '())
    (make-sequent '(a) '())
    (make-sequent '() '(a))
    (make-sequent '(a) '(a))
    (make-sequent '(a) '(b))
    (make-sequent '(a b c) '(d e f))
    (make-sequent '(or a (and b c)) '(and (or d e) f))
    (make-sequent '(and a b) '(and b a))
    (make-sequent '(or a b) '(or b a))
    (make-sequent '(then (not (or a b)) (and (not a) (not b))))
  ))

(map (compose display sequent->string) test-vectors)

(define test-vectors
  (list
    (make-sequent '(and a b) '(and b a))
    (make-sequent '(or a b) '(or b a))
    (make-sequent '(then (not (or a b)) (and (not a) (not b))))
    ; (make-sequent '(or a (and b c)) '(and (or d e) f))
  ))
(map (compose display rtree->string rewrite-tree) test-vectors)
