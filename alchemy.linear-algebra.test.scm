(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy linear-algebra))


(ma-pp (rho 3 4 '(1 2 3 4
                  5 6 7 8
                  9 10 11 12)))

(ma-pp (transpose (rho 3 4 '(1 2 3 4
                  5 6 7 8
                  9 10 11 12))))

(ma-pp (ma-swap-col! (rho 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12))
                     1 2))
