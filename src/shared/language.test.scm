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
        (alchemy language))


(display
  ((compose
     (lambda (x y) (+ x y))
     (lambda (x y z) (values (* x y) (* y z))))
   1 2 3))
(display "\n")

(display
  ((compose
    (applify +)
    (applify map *))
   '((1 2) (3 4))))
(display "\n")

(display
  ((compose
     (lambda (x y) (+ x y))
     (lambda (x y) (values (+ x y) (- y x)))
     (lambda (x y z) (values (* x y) (* y z))))
   1 2 3))
(display "\n")
