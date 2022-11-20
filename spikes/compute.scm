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

;; Here is an example for computational helpers soone can try an retry easly
(import (alchemy computation))

(define v (vector 1 2 3))
(define A (outer-product + v v))

; won't compute if there exists a file `tmp_data/L.dat` but instead load the result
(dump-step 'L (lll A))

(print L)

; will delete step only on computation success
(delete-step-on-success 'L)
