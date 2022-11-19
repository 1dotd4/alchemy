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
