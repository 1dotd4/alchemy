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

(define-library (alchemy graph)
  (export graph->dot)
  (import (scheme base)
          (alchemy language))
  (begin
    (define (graph->dot G)
      ;; Right now G := (nodes . edges)
      (define (edge->dot E)
        (string-append "  " (car E) " -> " (cdr E) ";\n"))
      (string-append "digraph G {\n" (fold string-append "" (map edge->dot (cdr G))) "}\n"))
    ))
