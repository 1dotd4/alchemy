;; This file is part of alchemy.
;; Copyright (c) 2022 unpx.net
;;
;; Licence??

;; import and glue together components here

(load "src/shared/cauldron.scm")
(load "src/shared/language.scm")
(load "src/shared/graph.scm")
(load "src/math/algebra.scm")
(load "src/math/number-theory.scm")
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
        (alchemy algebra)
        (alchemy number-theory)
        (alchemy linear-algebra)
        (alchemy encoding)
        (alchemy graph)
        (alchemy cipher)
        (alchemy cryptanalysis)
        (scheme repl)
        )

(interaction-environment)
