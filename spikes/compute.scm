;; Here is an example for computational helpers soone can try an retry easly
(import (alchemy computation))

(define v (vector 1 2 3))
(define A (outer-product + v v))

; won't compute if there exists a file `tmp_data/L.dat` but instead load the result
(dump-step 'L (lll A))

(print L)

; will delete step only on computation success
(delete-step-on-success 'L)
