(import (alchemy cauldron))

(steer "Divide by zero." (/ 5 0))
(steer-taste "Zero is zero." (zero? 0))
(steer-observe "One plus one is three." (+ 1 1) 3)
(steer-observe "Two plus two is four." (+ 2 2) 4)

