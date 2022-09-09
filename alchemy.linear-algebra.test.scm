(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy linear-algebra))


(ma-pp (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12)))

(ma-pp (transpose (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12))))

(ma-pp (ma-swap-col! (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12))
                     1 2))

(ma-pp (rho 2 2 '(2 3 5 6)))
(v-pp
  (square-linear-system
    (rho 2 2 '(2 3 5 6))
    (vector 4 7)))

(v-pp
  (square-linear-system
    (rho 4 4 '( 1  1 -3  1
               -5  3 -4  1
                1  0  2 -1
                1  2  0  0))
    (vector 2 0 1 12)))

