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

(import (alchemy cauldron))

(steer "Divide by zero." (/ 5 0))
(steer-taste "Zero is zero." (zero? 0))
(steer-observe "One plus one is three." (+ 1 1) 3)
(steer-observe "Two plus two is four." (+ 2 2) 4)
(steer-observe "Conses '(2 . 3) == '(2 . 3)" '(2 . 3) '(2 . 3))

