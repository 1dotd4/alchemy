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

(import
  (srfi-1)
  (chicken format))

;; J_n = {1, ..., n}
;; S_n = S(J_n) permutation of J_n
;; The vector (σ(1) ... σ(n)) is the permutation that takes each index i -> σ(i)

(define (permutation perm)
  (lambda (i) (list-ref perm (sub1 i))))

(define σ (permutation '(2 1 3)))

(define (pp perm)
  (print (map (o perm add1) (iota 3))))

(pp σ)

(define (orbit perm)
  (lambda (i)
    (let rec ((x i) (r (list i)))
      (let ((p (perm x)))
        (if (= i p)
          (reverse (cons i r))
          (rec p (cons p r)))))))

(print ((orbit σ) 2))

;; would be nice to have set theory here to disjoin stuff in orbits :3
