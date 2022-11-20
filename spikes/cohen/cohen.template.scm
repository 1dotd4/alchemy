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
