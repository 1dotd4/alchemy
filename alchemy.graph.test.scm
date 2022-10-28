(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy graph)
        (srfi 1))

; (define (member? lst obj) (not (null? (member obj lst))))
(define (insert obj set) (if (member obj set) set (cons obj set)))
(define (divisors n)
  (filter (lambda (x) (zero? (modulo n x))) (range 1 n)))

(define (divisor-graph n)
  (cons
    (range 1 (+ n 1))
    (fold
      insert
      '()
      (fold
        cons
        (map
          (lambda (d)
            (cons (number->string n)
                  (number->string d)))
          (divisors n))
        (concatenate
          (map
            (lambda (i)
              (map
                (lambda (d) (cons (number->string i)
                                  (number->string d)))
                (divisors i)))
            (divisors n)))))))

(define (divisor-graph-up-to n)
  (cons
    (range 1 (+ n 1))
    (fold
      (lambda (graph set)
        (fold
          insert
          set
          (cdr graph)))
      '()
      (map divisor-graph (range 2 (+ n 1))))))
;; TOOD: get a "union of graph"
;; TODO: get a "remove node"


; (display (divisor-graph 8))

; (display (graph->dot (divisor-graph-up-to 81)))
(display (graph->dot (divisor-graph 30)))
