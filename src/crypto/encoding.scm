;; This file is part of alchemy.
;; Copyright (c) 2022 unpx.net
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-library (alchemy encoding)
  (export *asciiPrintable*
          encode-grid pretty-print-grid
          encode-hex decode-hex
          encode-base64 decode-base64
          u8vector->string u8vector->safe-string)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (srfi 4)   ; Homogeneous numeric vector datatypes
          (srfi 151)
          (srfi 207) ; String-notated bytevectors
          (alchemy language)
          )
  (begin
    (define (encode-hex bytes)
      (bytevector->hex-string bytes))
    (define (decode-hex str)
      (hex-string->bytevector str))

    (define *alphaTable* (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (define *b64padding* (string->list "=="))

    (define (encode-base64 bytes)
      (bytevector->base64 bytes))
      ;(define (sextet->b64 sex)
      ;  (list-ref *alphaTable* sex))
      ;(define (encode-3 bytes)
      ;  (list (sextet->b64 (arithmetic-shift (car bytes) -2))
      ;        (sextet->b64 (bitwise-ior
      ;                       (arithmetic-shift (bitwise-and (car bytes) #x3) 4)
      ;                       (arithmetic-shift (cadr bytes) -4)))
      ;        (sextet->b64 (bitwise-ior
      ;                       (arithmetic-shift (bitwise-and (cadr bytes) #xf) 2)
      ;                       (arithmetic-shift (list-ref bytes 2) -6)))
      ;        (sextet->b64 (bitwise-and (list-ref bytes 2) #x3f))))
      ;(define (encode-2 bytes)
      ;  (list (sextet->b64 (arithmetic-shift (car bytes) -2))
      ;        (sextet->b64 (bitwise-ior
      ;                       (arithmetic-shift (bitwise-and (car bytes) #x3) 4)
      ;                       (arithmetic-shift (cadr bytes) -4)))
      ;        (sextet->b64 (arithmetic-shift (bitwise-and (cadr bytes) #xf) 2))))
      ;(define (encode-1 byte)
      ;  (list (sextet->b64 (arithmetic-shift byte -2))
      ;        (sextet->b64 (arithmetic-shift (bitwise-and byte #x3) 4))))
      ;(define (b64pad str)
      ;  (cond
      ;    ((= (modulo (length str) 4) 0)
      ;     str)
      ;    ((= (modulo (length str) 4) 3)
      ;     (append str (take *b64padding* 1)))
      ;    ((= (modulo (length str) 4) 2)
      ;     (append str (take *b64padding* 2)))
      ;    (else (error "WTF?"))))
      ;(let rec ((remaining (u8vector->list bytes))
      ;          (processed '()))
      ;  (cond
      ;    ((>= (length remaining) 3) 
      ;     (rec
      ;       (drop remaining 3)
      ;       (append
      ;         processed
      ;         (encode-3 (take remaining 3)))))
      ;    ((= (length remaining) 2) 
      ;     (rec
      ;       (drop remaining 2)
      ;       (append
      ;         processed
      ;         (encode-2 (take remaining 2)))))
      ;    ((= (length remaining) 1) 
      ;     (rec
      ;       (drop remaining 1)
      ;       (append
      ;         processed
      ;         (encode-1 (car remaining)))))
      ;    (else
      ;      (list->string (b64pad processed))))))

    (define (decode-base64 str)
      ;; https://rosettacode.org/wiki/Base64_decode_data#Haskell
      (error "TODO"))

    (define *asciiPrintable*
      ;(string->list "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c"))
      (string->list "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r"))

    (define (half-nibble->unicode n)
      (list-ref '(" " "▄" "▀" "█") n))

    (define (nibble->unicode n)
      (string-append
        (half-nibble->unicode (quotient n 4))
        (half-nibble->unicode (modulo n 4))))

    ; currently accepting number, should be byte.
    ; then have a integer->u8vector
    (define encode-grid
      (case-lambda
        ((n) (encode-grid n 0))
        ((n pad)
         (if (zero? n)
           (if (<= pad 0) ""
             (apply string-append (make-list pad "  ")))
           (string-append
             (encode-grid
               (quotient n 16)
               (- pad 1))
             (nibble->unicode
               (modulo n 16)))))))

    (define (pretty-print-grid . args)
      (display
        (string-append
          "["
          (apply encode-grid args)
          "]\n")))

    (define (u8vector->string bytes)
      (list->string (map integer->char (u8vector->list bytes))))
    (define (u8vector->safe-string bytes)
      (list->string
        (map
          (lambda (c)
            (if (member c *asciiPrintable*)
              c
              #\?))
          (map integer->char (u8vector->list bytes)))))


    ; (define (number->hex nbits val)
    ;     (format (string-append "~" (format "~D" nbits) ",48X") val))

  ))
