(define-library (alchemy cryptanalysis)
  (export freq-count
          attack-single-xor-cipher-english
          )
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 4)
          (srfi 95)
          (srfi 152)
          (srfi 207)
          (alchemy encoding)
          (alchemy cipher)
          )
  (begin
    (define (add1 n) (+ n 1))
    (define (alist-set key value alist)
      (alist-cons key value
                  (filter (lambda (aval) (not (equal? key (car aval)))) alist)))

    (define (freq-count bytes)
      (sort
        (let fold ((bytelist bytes)
                   (afreq '()))
          (if (null? bytelist)
            afreq
            (fold (cdr bytelist)
                  (let* ((char (car bytelist))
                         (maybe (assoc char afreq)))
                    (if maybe
                      (alist-set char (add1 (cdr maybe)) afreq)
                      (alist-cons char 1 afreq))))))
        (lambda (x y)
          (> (cdr x) (cdr y)))))

    (define (upper-case text)
      (list->u8vector
        (map (lambda (x)
               (cond
                 ((and (>= x 97) (<= x 122)) (- x 32))
                 (else x)))
             (u8vector->list text))))

    (define *english-freqs*
      ; TODO: build from a book.
      ; Note: space is important.
      ; do we need it to be complete?
      (map
        (lambda (x)
          (cons
            (char->integer (car (string->list (car x))))
            (cdr x)))
      '(("E" .  13.0)  ("T" .  9.1) ("A" .  8.2) ("N" .  7.5) ("O" .  7.0) ("I" .  6.7)
        ("S" .   6.3)  ("H" .  6.1) ("R" .  6.0) ("D" .  4.3) ("L" .  4.0) ("U" .  2.8)
        ("C" .   2.8)  ("M" .  2.4) ("W" .  2.4) ("F" .  2.2) ("G" . 2.0)  ("Y" . 2.0)
        ("P" .   1.9)  ("B" .  1.5) ("V" .  0.9) ("K" . 0.77) ("J" . 0.15) ("X" . 0.15)
        ("Q" . 0.095) ("Z" . 0.074) (" " . 1)
        )))

    (define (english-score text)
      (define (score char dictionary)
        (let ((maybe (assoc char dictionary)))
          (if maybe (cdr maybe)
            (if (member char *asciiPrintable*) 0 -10))))
      (let ((freqs (freq-count (u8vector->list (upper-case text)))))
        (cons 
          (u8vector->safe-string text)
          (/
            (fold + 0
                  (map
                    (lambda (x)
                      (* (score (car x) *english-freqs*)
                         (cdr x)))
                    freqs))
            (length freqs)))))

    (define (attack-single-xor-cipher-english ct)
      (car
        (sort
          (map english-score
               (map (lambda (key) (xor-byte ct key)) (iota 256)))
          (lambda (a b) (> (cdr a) (cdr b))))))


    ))
