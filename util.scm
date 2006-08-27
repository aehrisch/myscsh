(define (string->byte-vector str)
  (byte-vector-ec (:string c str)
    (char->ascii c)))

(define (byte-vector->string bv)
  (string-ec (:byte-vector b bv)
    (ascii->char b)))

;;; code to add numbers modulo 2^32

(define ring-size (expt 2 32))

(define (mod2+ a b)
  (modulo (+ a b) ring-size))

(define (mod5+ a b c d e)
  (mod2+ (mod2+ (mod2+ (mod2+ a b) c) d) e))

(define (mod2* a b)
  (modulo (* a b) ring-size))

;;; *sigh*
(define (floor-int float)
  (inexact->exact (floor float)))