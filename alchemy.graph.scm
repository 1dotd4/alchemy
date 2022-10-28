(define-library (alchemy graph)
  (export graph->dot)
  (import (scheme base)
          (srfi 1))
  (begin
    (define (graph->dot G)
      ;; Right now G := (nodes . edges)
      (define (edge->dot E)
        (string-append "  " (car E) " -> " (cdr E) ";\n"))
      (string-append "digraph G {\n" (fold string-append "" (map edge->dot (cdr G))) "}\n"))
    ))