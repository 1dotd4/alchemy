(import (alchemy cauldron)
        (alchemy algebra)
        (alchemy number-theory))

(steer-observe "φ(16 = 2^4) = 2^3." (phi 16) 8)
(steer-observe "φ(6 = 3*2) = 2." (phi 6) 2)
(steer-observe "φ(5) = 5 - 1." (phi 5) 4)

(steer-observe "Tonelli 11" (tonelli 5 11) '(4 . 7))
(steer-observe "Tonelli 41" (tonelli 5 41) '(28 . 13))
(steer-observe "Tonelli big."
               (let ((a 8479994658316772151941616510097127087554541274812435112009425778595495359700244470400642403747058566807127814165396640215844192327900454116257979487432016769329970767046735091249898678088061634796559556704959846424131820416048436501387617211770124292793308079214153179977624440438616958575058361193975686620046439877308339989295604537867493683872778843921771307305602776398786978353866231661453376056771972069776398999013769588936194859344941268223184197231368887060609212875507518936172060702209557124430477137421847130682601666968691651447236917018634902407704797328509461854842432015009878011354022108661461024768)
                     (p 30531851861994333252675935111487950694414332763909083514133769861350960895076504687261369815735742549428789138300843082086550059082835141454526618160634109969195486322015775943030060449557090064811940139431735209185996454739163555910726493597222646855506445602953689527405362207926990442391705014604777038685880527537489845359101552442292804398472642356609304810680731556542002301547846635101455995732584071355903010856718680732337369128498655255277003643669031694516851390505923416710601212618443109844041514942401969629158975457079026906304328749039997262960301209158175920051890620947063936347307238412281568760161))
               (tonelli a p))
               '(2362339307683048638327773298580489298932137505520500388338271052053734747862351779647314176817953359071871560041125289919247146074907151612762640868199621186559522068338032600991311882224016021222672243139362180461232646732465848840425458257930887856583379600967761738596782877851318489355679822813155123045705285112099448146426755110160002515592418850432103641815811071548456284263507805589445073657565381850521367969675699760755310784623577076440037747681760302434924932113640061738777601194622244192758024180853916244427254065441962557282572849162772740798989647948645207349737457445440405057156897508368531939120 . 28169512554311284614348161812907461395482195258388583125795498809297226147214152907614055638917789190356917578259717792167302913007927989841763977292434488782635964253677743342038748567333074043589267896292373028724763808006697707070301035339291758998923066001985927788808579330075671953036025191791621915640175242425390397212674797332132801882880223506177201168864920484993546017284338829512010922075018689505381642887042980971582058343875078178836965895987271392081926458392283354971823611423820865651283490761548053384731721391637064349021755899877224522161311561209530712702153163501623531290150340903913036821041))

(steer-taste "7 is prime." (prime? 7))
(steer-taste "2 is prime." (prime? 2))
(steer-taste "8 is not prime." (not (prime? 8)))
(steer-taste "1 is not prime." (not (prime? 1)))
(steer-taste "A large prime wolfram." (prime? 4827680675583175607010711196835010234688975248888438745750998172824341205854547076737726430748789131))
(steer-taste "A large prime wikipedia." (prime? 29674495668685510550154174642905332730771991799853043350995075531276838753171770199594238596428121188033664754218345562493168782883))
(steer-taste "A charmichael is not a prime."
             (let ((p 29674495668685510550154174642905332730771991799853043350995075531276838753171770199594238596428121188033664754218345562493168782883))
               (not (prime?
                 (* p
                    (+ (* 313 (- p 1)) 1)
                    (+ (* 353 (- p 1)) 1))))))

(steer-observe "xgcd 240 46." (xgcd integer-ring 240 46) '(-9 47 2))
(steer-observe "5^3 mod 14 = 13." (square-multiply (make-ring-modulo 14) 5 3) 13)
(steer-observe "3^4 mod 7 = 4" (square-multiply (make-ring-modulo 7) 3 4) 4)


