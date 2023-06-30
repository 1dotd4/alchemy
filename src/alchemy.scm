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

;; import and glue together components here

(load "src/shared/cauldron.scm")
(load "src/shared/language.scm")
(load "src/shared/rewrite.scm")
(load "src/shared/graph.scm")
(load "src/math/algebra.scm")
(load "src/math/number-theory.scm")
(load "src/math/factoring.scm")
(load "src/math/linear-algebra.scm")
(load "src/math/polynomials.scm")
(load "src/crypto/encoding.scm")
(load "src/crypto/cipher.scm")
(load "src/crypto/cryptanalysis.scm")

; (display (string-append "Loaded version " *version*))
(define *version* "alchemy 0.1b1\n")
(define (reload)
  (load "src/alchemy.scm"))

(import (alchemy cauldron)
        (alchemy language)
        (alchemy rewrite)
        (alchemy algebra)
        (alchemy number-theory)
        (alchemy polynomials)
        (alchemy factoring)
        (alchemy linear-algebra)
        (alchemy encoding)
        (alchemy graph)
        (alchemy cipher)
        (alchemy cryptanalysis)
        (scheme repl)
        )

(interaction-environment)
