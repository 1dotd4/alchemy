(define-library (alchemy cipher)
  (export xor xor-byte xor-key)
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 4)
          (srfi 151) ; Bitwise Operations
          )
  (begin
    (define (xor a b)
      (list->u8vector
        (map (lambda (x) (bitwise-xor (car x) (cadr x)))
             (zip (u8vector->list a) (u8vector->list b)))))

    (define (xor-byte a k)
      (list->u8vector
        (map (lambda (x) (bitwise-xor x k))
             (u8vector->list a))))

    (define (xor-key key bytes)
      (let rec ((remaining bytes)
                (processed (xor bytes key))
                (result (list->u8vector '())))
        (if (zero? (length (u8vector->list processed)))
          result
          (let ((new-rem (list->u8vector (drop (u8vector->list remaining) (length (u8vector->list processed))))))
            (rec new-rem
                 (xor new-rem key)
                 (list->u8vector (append (u8vector->list result) (u8vector->list processed))))))))


    ))
