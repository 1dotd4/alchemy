;; import and glue together components here

(load "version.scm")
(load "helper.scm")
(load "alchemy.cauldron.scm")
(load "alchemy.number-theory.scm")
(load "alchemy.encoding.scm")
(load "alchemy.cipher.scm")
(load "alchemy.cryptanalysis.scm")

(display (string-append "Loaded version " *version*))

(import (alchemy cauldron)
        (alchemy number-theory)
        (alchemy encoding)
        (alchemy cipher)
        (alchemy cryptanalysis)
        )
