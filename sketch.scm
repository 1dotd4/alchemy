(import scheme
        (chicken base)
        (chicken repl)
        (chicken io)
        srfi-1
        miscmacros
        (clojurian syntax)
        ;; (clojurian atom)
        utf8
        srfi-13
        linenoise
        tcp6
        test)

;;; ===[ Introduction ]===

;;; https://www.scheme.com/tspl4/control.html#./control:h6

(define (inspect i)
  (print i)
  i)

;;; generic square multiply
(define (generate-square-multiply mult-fn id)
  (define (sq-mult base exponent)
    (define (internal-sq-mult base exponent result) 
      (cond ((< exponent 0) #f)
            ((= exponent 0) result)
            ((zero? (modulo exponent 2))
              (internal-sq-mult (mult-fn base base)
                                (quotient exponent 2)
                                result))
            (else
              (internal-sq-mult (mult-fn base base)
                                (quotient exponent 2)
                                (mult-fn base result)))))
    (internal-sq-mult base exponent id))
  sq-mult)

;;; base ^ exponent mod n
(define (modexpt base exponent mod)
  (define (mult-fn a b) (modulo (* a b) mod))
  ((generate-square-multiply mult-fn 1) base exponent))

;; Legendre Symbol
(define (legendreSymbol a p)
  (let ((power (modexpt a (quotient (sub1 p) 2) p)))
  (if (> power 1)
      -1
      power)))


;; Tonelli Square root modulo
(define (tonelli n p) 
  ;; Step 0. Check that n is indeed square: (n | p) must be ≡ 1
  (letrec ((check (lambda (a p)
                    (if (= (legendreSymbol a p) 1)
                        (setup a p)
                        #f)))
      

           ;; Setup
           ;; Step 1. Factors out powers of 2 from p-1
           ;; Define q -odd- and s such as p-1 = q * 2^s
           ;; Step 2. Select a non-square z such as (z | p) = -1
           ;; and set c ≡ z^q
           ;; Step 3. Set r ≡ n^((q+1)/2), t ≡ n^q, m = s
           (findQandS (lambda (p s)
                        (if (zero? (remainder p 2))
                            (findQandS (quotient p 2) (add1 s))
                            (cons p s))))
           (findz (lambda (n i)
                    (if (= -1 (legendreSymbol i n))
                        i
                        (findz n (add1 i)))))
           (setup (lambda (n p)
                    (let* ((qands (findQandS (sub1 p) 0))
                            (q (car qands))
                            (s (cdr qands)))
                      
                      (loop n
                            p
                            s
                            (modexpt (findz p 0) q p)
                            (modexpt n q p)
                            (modexpt n (quotient (add1 p) 2) p)))))
           

           ;; Step 4. Loop.
           ;; If t ≡ 1 output r, p-r.
           ;; n p m c t r
           ;; Otherwise find, by repeated squaring,
           ;;   the lowest i, 0 < i < m, such as t^(2^i) ≡ 1
           ;; Let b ≡ c^(2^(m-i-1)), and set r ≡ r*b,
           ;; t ≡ t*b^2, c ≡ b^2 and m = i.
           (lowesti (lambda (t p i)
                      (if (zero? (modulo (sub1 t) p))
                          i
                          (lowesti (modexpt t 2 p) p (add1 i)))))
           (loop (lambda (n p m c t r)
                    (let* ((i (lowesti (modexpt t 2 p) p 1))
                           (b (modexpt c (expt 2
                                               (if (> (- m i 1) 0)
                                                   (- m i 1)
                                                   0))
                                       p)))
                      (if (zero? (modulo (sub1 t) p))
                          (cons r (- p r))
                          (loop n
                                p
                                i
                                (modulo (* b b)   p)
                                (modulo (* t b b) p)
                                (modulo (* r b)   p)))))))
  ;; Start the tonelli algorithm.
  (check n p)))

(define (test-tonelli)
  (begin 
    (let ((a 8479994658316772151941616510097127087554541274812435112009425778595495359700244470400642403747058566807127814165396640215844192327900454116257979487432016769329970767046735091249898678088061634796559556704959846424131820416048436501387617211770124292793308079214153179977624440438616958575058361193975686620046439877308339989295604537867493683872778843921771307305602776398786978353866231661453376056771972069776398999013769588936194859344941268223184197231368887060609212875507518936172060702209557124430477137421847130682601666968691651447236917018634902407704797328509461854842432015009878011354022108661461024768)
          (p 30531851861994333252675935111487950694414332763909083514133769861350960895076504687261369815735742549428789138300843082086550059082835141454526618160634109969195486322015775943030060449557090064811940139431735209185996454739163555910726493597222646855506445602953689527405362207926990442391705014604777038685880527537489845359101552442292804398472642356609304810680731556542002301547846635101455995732584071355903010856718680732337369128498655255277003643669031694516851390505923416710601212618443109844041514942401969629158975457079026906304328749039997262960301209158175920051890620947063936347307238412281568760161))
      (print (tonelli a p)))))

;; Note to self, get a factor function and simplify this trivial
;; division.
(define (phi n)
  (define (fit-number a b)
    (define (f a b r)
      (if (zero? (quotient a b))
          r
          (if (zero? (remainder a b))
              (f (quotient a b) b (add1 r))
              r)))
    (f a b 0))
  (define (until-factored n i r)
    (if (<= i n)
      (if (zero? (remainder n i)) ;; aka divides
          (let ((k (fit-number n i)))
            (until-factored (quotient n (expt i k))
                            (add1 i)
                            (* r (- i 1) (expt i (sub1 k)))))
          (until-factored n (add1 i) r))
      r))
  (until-factored n 2 1))


;; ===[ Modular Arithmetic ]===

;; modular numbers
(define (make-mod a n)
  (list a n))

(define (m->number a) (car a))
(define (m->modulo a) (cadr a))

(define (m-zero? a) (zero? (m->number a)))

(define (quotRem a b)
  (list
    (quotient a b)
    (remainder a b)))

(define (egcd a b)
  (if (< b 1)
    (list 1 1 0)
    (let* ((q (quotient a b))
             (r (remainder a b))
             (e (egcd b r))
             (s (cadr e))
             (t (caddr e))
             (v (- s (* q t))))
      (list
        (+ (* a t) (* b v))
        t
        v))))

(define (m->neg m)
  (let* ((n (m->modulo m))
         (a (m->number m)))
    (make-mod (- a) n)))

(define (m->inv a)
  (let* ((i (m->number a))
        (n (m->modulo a))
        (e (egcd i n))
        (x (cadr e))
        (y (caddr e)))
    (if (eq? 1
             (+ (* i x)
                (* n y)))
        (make-mod
          (if (positive? x)
              x
              (+ n x))
          n)
        (begin
          (print "cannot get inverse of " i " mod " n)
          #f))))

(define (m+ a . bs)
  (let ((m (m->modulo a)))
    (make-mod
      (modulo (apply + (map m->number (cons a bs))) m)
      m)))

(define (m- a . bs)
  (let ((m (m->modulo a)))
    (make-mod
      (modulo (apply - (map m->number (cons a bs))) m)
      m)))

(define (m* a . bs)
  (let ((m (m->modulo a)))
    (make-mod
      (modulo (apply * (map m->number (cons a bs))) m)
      m)))

(define (m/ a . bs)
  (let ((m (m->modulo a)))
    (apply m* (cons a (map m->inv bs)))))

(define (m^ a e)
  (let ((n (m->modulo a)))
    (begin
      (define (m-expt a e r)
        (cond ((< e 0) (make-mod 1 n))
              ((eq? e 0) r)
              ((zero? (modulo e 2)) (m-expt (m* a a) (quotient e 2) r))
              (else (m-expt (m* a a) (quotient e 2) (m* a r)))))
      (m-expt a e (make-mod 1 n)))))

;; ===[ Group theory library ]===
;;
;; A group is a set with a binary composition function with the
;; closure property and the following axioms:
;;
;; associativity)                 (a * b) * c = a * (b * c)
;; an identity element)           a * e = a = e * a
;; inverse for each element)      a * a^-1 = e = a^-1 * a
;;
;; We choose to rappresent the group as follow:
;; the principal data we need is
;;   - a set                  ( _|_   -> S )
;;   - a identity function    ( S     -> I )
;;   - a composition function ( S x S -> S )
;;   - an inverse function    ( S     -> S )

(define (mk-group set id composition inverse) 
  (list set id composition inverse))

(define (generate-list-to n)
  (define (cons-to-next-from l i)
    (if (< i 1)
      (cons 0 l)
      (cons-to-next-from (cons i l) (sub1 i))))
  (cons-to-next-from '() n))

(define (generate-list-gcd1-to n)
  (define (cons-to-n l i)
    (cond ((zero? i)                l)
          ((zero? (sub1 (gcd i n))) (cons-to-n (cons i l) (sub1 i)))
          (else                     (cons-to-n l (sub1 i)))))
  (cons-to-n '() (sub1 n)))


(define (mk-integer-group-modulo n)
  (if (< n 1)
      #f
      (mk-group (generate-list-to (sub1 n))
                0
                (lambda (a b) (modulo (+ a b) n))
                (lambda (a)   (modulo (- n a) n)))))

(define (mk-multiplicative-group n)
  (if (< n 1)
    #f
    (mk-group (generate-list-gcd1-to (sub1 n))
              1
              (lambda (a b) (modulo (* a b) n))
              (lambda (a)   (modexpt a (sub1 (phi n)) n)))))
              

(define (group:elements g) (car g))
(define (group:identity g) (cadr g))
(define (group:composition g) (caddr g))
(define (group:inverse g) (cadddr g))

(define (group:order g)
  (lambda (a)
    (define (count-to-identity r i)
      ;; maybe terminate at (< (length (group:elements g)) i) #f
      (if (not (= (group:identity g) r))
          (count-to-identity ((group:composition g) r a) (add1 i))
          i))
    (count-to-identity a 1)))

(define (group:expt g)
  (generate-square-multiply (group:composition g) (group:identity g)))

(define (generate-subgroup-from-element group given-element)
  (define (add-till-identity current-element elements)
    (let ((id (group:identity group))
          (comp (group:composition group)))
      (if (not (= id current-element))
          (add-till-identity (comp given-element current-element)
                             (cons current-element elements))
          (cons id elements))))
  (mk-group (add-till-identity given-element '())
            (group:identity group)
            (group:composition group)
            (group:inverse group)))


;; simple test
(define (test-integer-group)
  (let* ((g (mk-integer-group-modulo 7))
         (id (group:identity g))
         (c (group:composition g))
         (inv (group:inverse g))
         (ord (group:order g))
         (pow (group:expt g)))
    (print id)
    (print (c 4 6))
    (print (inv 5))
    (print (ord 5))
    (print (ord 0))
    (print (ord 2))
    (print (pow 2 3))
  ))


(define (test-group-theory)
  (test-group    "exptMod"
    (test-assert "negative"     (not (modexpt 2 -1  10)))
    (test-assert "correct expt" (=   (modexpt 2  3  10)   (expt 2 3)))
    (test-assert "pow 0"        (=   (modexpt 2  0  10)   1))
    (test-assert "pow 1"        (=   (modexpt 2  1   5)   2))
    (test-assert "pow mod"      (=   (modexpt 2  3   5)   (modulo (expt 2 3) 5)))
    ))

;;;; toadstyle 57. Diffie-Hellman Revisited: Subgroup-Confinement Attacks
(define p 7199773997391911030609999317773941274322764333428698921736339643928346453700085358802973900485592910475480089726140708102474957429903531369589969318716771)
(define g 4565356397095740655436854503483826832136106141639563487732438195343690437606117828318042418238184896212352329118608100083187535033402010599512641674644143)
(define q 236234353446506858198510045061214171961) ; order of g mod p

(define j 30477252323177606811760882179058908038824640750610513771646768011063128035873508507547741559514324673960576895059570) ; (p-1) / q

;; todo factors < 2^16 and no repeated factors

;; Pohlig-Hellman algorithm for discrete logarithms
; 1. Take one of the small factors j. Call it r. We want to
; find an element h of order r. To find it, do:
;     h := rand(1, p)^((p-1)/r) mod p
; If h = 1, try again.
; 2. You're Eve. Send Bob h as your public key. Note that h
; is not a valid public key! There is no x such that h = g^x
; mod p. But Bob doens't know that.
; 3. Bob will compute:
;     K := h^x mod p
; Where x is his secret key and K is the output shared
; secret. Bob then sends back (m, t), with:
;     m := "crazy flamboyant for the rap enjoyment"
;     t := MAC(K, m)
; 4. We (Eve) can't compute K, because h isn't actually a
; valid public key. But we're not licked yet.
; Remember how we saw that g^x starts repeating when x > q?
; h has the same property with r. This means there are only
; r possible values of K that Bob could have generated. We
; can revocer K by doing a brute-force search over there
; values until t = MAC(K, m).
; Now we know Bob's secret key x mod r.
; 5. Repeat steps 1 through 4 many times. Eventually you
; will know:
;     x = b1 mod r1
;     x = b2 mod r2
;     x = b3 mod r3
;     ...
; Once (r1*r2*r3*...*rn) > q, you'll have enough information
; to reassemble Bob's secret key using the Chinese Remainder
; Theorem.

;;;; Elliptic curves simplified
(define (make-point-bis n x y)
  (list (make-mod x n)
          (make-mod y n)))
(define (point-bis->x p)
  (car p))
(define (point-bis->y p)
  (cadr p))
(define (point-bis->mod p)
  (m->modulo (point->x p)))

(define (elliptic-curve-bis n a b)
  (define (elliptic-inverse p)
    (make-point-bis
      n
      (point-bis->x p)
      (m->neg (point-bis->y p))))
  (define (elliptic-add first-point . other-points)
    (define (internal-add p1 p2)
      (cond
        ;; to do check points are in curve
        ;; ((not (eq? (point->curve p1) (point->curve p2))) #f)
        ((m-zero? (point-bis->x p1)) p2)
        ((m-zero? (point-bis->y p1)) p2)
        ((m-zero? (point-bis->x p2)) p1)
        ((m-zero? (point-bis->y p2)) p1)
        ((eq? (elliptic-inverse p1) (elliptic-inverse p2))
          (make-point-bis n 0 0))
        (else
          (let* ((l (if (and (eq? (point-bis->x p1) (point-bis->x p2))
                            (eq? (point-bis->y p1) (point-bis->y p2)))
                        (m/
                          (m+ (m* (make-mod 3 n)
                                  (point-bis->x p1)
                                  (point-bis->x p1))
                              (make-mod a n))
                          (m* (make-mod 2 n)
                              (point-bis->y p1)))
                        (m/
                          (m- (point-bis->y p2)
                              (point-bis->y p1))
                          (m- (point-bis->x p2)
                              (point-bis->x p1)))))
                (x (m- (m^ l 2)
                        (point-bis->x p1)
                        (point-bis->x p2)))
                (y (m- (m* l
                            (m- (point-bis->x p1)
                                x))
                        (point-bis->y p1))))
            (make-point-bis n (m->number x) (m->number y))))))
    (fold internal-add first-point other-points))
  (mk-group `(,n ,a ,b) ;; Should say that this is for extra data, not just for sets
            (make-point-bis n 0 0)
            elliptic-add         
            elliptic-inverse))

(define (elliptic-curve n a b)
  (list n a b))
(define (curve->n curve)
  (car curve))
(define (curve->a curve)
  (cadr curve))
(define (curve->b curve)
  (caddr curve))

(define (make-point curve x y)
  (let ((n (curve->n curve)))
    (list curve
          (make-mod x n)
          (make-mod y n))))
(define (point->curve p)
  (car p))
(define (point->x p)
  (cadr p))
(define (point->y p)
  (caddr p))

(define (is-valid-curve? curve)
  (let ((a (curve->a curve))
        (b (curve->b curve)))
    (zero?
      (+
        (* 4 a a a)
        (* 27 b b)))))

(define (elliptic-inverse p)
  (make-point
    (point->curve p)
    (point->x p)
    (m->neg (point->y p))))

(define (elliptic-add first-point . other-points)
  (define (internal-add p1 p2)
    (cond
      ((not (eq? (point->curve p1) (point->curve p2))) #f)
      ((m-zero? (point->x p1)) p2)
      ((m-zero? (point->y p1)) p2)
      ((m-zero? (point->x p2)) p1)
      ((m-zero? (point->y p2)) p1)
      ((eq? (elliptic-inverse p1) (elliptic-inverse p2))
        (make-point point->curve 0 0))
      (else
        (let* ((n (curve->n (point->curve p1)))
               (a (curve->a (point->curve p1)))
               (l (if (and (eq? (point->x p1) (point->x p2))
                           (eq? (point->y p1) (point->y p2)))
                       (m/
                         (m+ (m* (make-mod 3 n)
                                 (point->x p1)
                                 (point->x p1))
                             (make-mod a n))
                         (m* (make-mod 2 n)
                             (point->y p1)))
                       (m/
                         (m- (point->y p2)
                             (point->y p1))
                         (m- (point->x p2)
                             (point->x p1)))))
               (x (m- (m^ l 2)
                      (point->x p1)
                      (point->x p2)))
               (y (m- (m* l
                          (m- (point->x p1)
                              x))
                      (point->y p1))))
          (make-point (point->curve p1) (m->number x) (m->number y))))))
  (fold internal-add first-point other-points))

(define (elliptic-scalar point exponent)
  (let ((n (curve->n (point->curve point)))
        (identity (make-point (point->curve point) 0 0)))
    ((generate-square-multiply elliptic-add identity) point exponent)))

;; TODO: (elliptic-lift-x curve x) lift a point given it's x coordinate
(define (elliptic-lift-x curve x)
  (let* ((data (car curve))
         (n (car data))
         (a (cadr data))
         (b (caddr data)))
    (tonelli
      (remainder
        (+ (expt x 3)
          (* (expt x 2)
              a)
          (* b x))
        n)
      n)))

;; example
(define my-curve (elliptic-curve 9739 497 1768))
(define X (make-point my-curve 5274 2841))
(define Y (make-point my-curve 8669 740))
(print (elliptic-add X X))
(print (elliptic-add X Y))
(define P (make-point my-curve 493 5564))
(define Q (make-point my-curve 1539 4742))
(define R (make-point my-curve 4403 5202))
(print (elliptic-add P P Q R))
;; (define G (make-point my-curve 1804 5368))
(define QA (make-point my-curve 815 3190))
(print (elliptic-scalar QA 1829))


(print "example bis")
;; example-bis
(define N 9739)
(define my-curve-bis (elliptic-curve-bis N 497 1768))
(define X (make-point-bis N 5274 2841))
(define Y (make-point-bis N 8669 740))
(print ((group:composition my-curve-bis) X X))
(print ((group:composition my-curve-bis) X Y))
(define P (make-point-bis N 493 5564))
(define Q (make-point-bis N 1539 4742))
(define R (make-point-bis N 4403 5202))
(print ((group:composition my-curve-bis) P P Q R))
;; G?
(define QA (make-point-bis N 815 3190))
(print ((group:expt my-curve-bis) QA 1829))

(define n (- (expt 2 255) 19))
(define x 9)
;; Problem: this should be in the Montgomery form, not Weierstrass.
;; Thus y^2 = x^3 + 486662 * x^2 + x mod 2^255 - 19.
;; This imply that the lift function and the addition, doubling and power
;; must be reviewd.
;; See https://cryptohack.org/courses/elliptic/ladder/
(define curve (elliptic-curve-bis n 486662 1))
(define y (cdr (elliptic-lift-x curve x)))
(print y)
(define g (make-point-bis n x y))
(print (car (car ((group:expt curve) g #x1337c0decafe))))

;;;; Polynomials

(define (make-monomial coef degree)
  (list coef degree))

(define (monomial->coeff monomial)
  (car monomial))

(define (monomial->degree monomial)
  (cadr monomial))

(define (make-polynomial coeffs)
  (define (fill-polynomial polynomial coeffs-to-add current-degree)
    (if (null? coeffs-to-add)
        polynomial
        (fill-polynomial
          (cons (make-monomial (car coeffs-to-add) current-degree)
                polynomial)
          (cdr coeffs-to-add)
          (add1 current-degree))))
  (fill-polynomial '() (reverse coeffs) 0))

(define (add-monomial-to-poly amonom apoly)
  (define (internal-add apoly amonom result)
    (cond
      ((null? apoly) (append (list amonom)))
      ((> (monomial->degree amonom) (monomial->degree (car apoly)))
        (append result (list amonom) apoly))
      ((eq? (monomial->degree (car apoly)) (monomial->degree amonom))
        (append
          result
          (list (make-monomial (+ (monomial->coeff amonom)
                                  (monomial->coeff (car apoly)))
                              (monomial->degree amonom)))
          (cdr apoly)))
      (else
        (internal-add
          (cdr apoly)
          amonom
          (append result (list (car apoly)))))))
  (internal-add apoly amonom '()))

(define (polynomial+ apoly . bpoly)
  (fold
    add-monomial-to-poly
    apoly
    (apply append bpoly)))

;;; ===[ Network ]===

;; Simple example to try conn/wp
;; $ nc -l 127.0.0.1 1337
;; (connect/wp "127.0.0.1:1337" nc-example)
(define (nc-example i o)
  (write-line "Connected, write a line." o)
  (print (read-line i))
  (write-line "Yup I received it, bye." o)
  )

;;; Connect with procedure (abbreviated to conn/wp)
;; try to connect and if procedure has an gracefully close the connection
;; procedure must accept two arguments, in- and out-port
(define (conn/wp host-and-port procedure)
  (define-values (in-port out-port) (tcp-connect host-and-port))
  (dynamic-wind
    (lambda () #f)
    (lambda () (procedure in-port out-port))
    (lambda () (begin
                  (close-input-port in-port)
                  (close-output-port out-port)))))

;;; ===[ Appendix ]===

;;; Reload function
(define (reload)
  (load "alchemy.scm"))

;;; The repl

;;; curr-run will reset on reload
(set! curr-run 0)

(current-input-port (make-linenoise-port))
(define (repl-prompt)
  (lambda ()
    (begin
      (set! curr-run (add1 curr-run))
      ;;; display the prompt
      (string-join (list ";; " (number->string curr-run) ". ") ""))))

(repl)
