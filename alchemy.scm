;; import and glue together components here

(load "version.scm")
(load "helper.scm")
(load "alchemy.cauldron.scm")
(load "alchemy.language.scm")
(load "alchemy.algebra.scm")
(load "alchemy.number-theory.scm")
(load "alchemy.linear-algebra.scm")
(load "alchemy.polynomials.scm")
(load "alchemy.encoding.scm")
(load "alchemy.cipher.scm")
(load "alchemy.cryptanalysis.scm")

(display (string-append "Loaded version " *version*))

(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy number-theory)
        (alchemy linear-algebra)
        (alchemy encoding)
        (alchemy cipher)
        (alchemy cryptanalysis)
        )
