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

(import (alchemy cauldron)
        (alchemy language)
        (alchemy algebra)
        (alchemy graph))

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
