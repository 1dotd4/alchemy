(import (alchemy cauldron)
        (alchemy language))

(display
  ((a:compose
     (a:map (lambda (x) (+ x 2)))
     (a:map (lambda (x) (* x 2))))
   '(2 3 4)))
(display "\n")


