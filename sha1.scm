;;; ,open bitwise byte-vectors ascii srfi-28

(define (string->byte-vector str)
  (let* ((len (string-length str))
	 (bv (make-byte-vector len 0)))
    (do ((i 0 (+ i 1)))
	((>= i len) bv)
      (byte-vector-set! bv i 
			(char->ascii
			 (string-ref str i))))))

;;; make a mask with COUNT 1 bits shifted START bits left
(define (make-extract-mask start count)
  (let lp ((count count) (mask 0))
    (if (zero? count)
	(arithmetic-shift mask start)
	(lp (- count 1) 
	    (bitwise-ior (arithmetic-shift mask 1)
			 #b1)))))

;;; mask to extract the first 32 bits
(define first-32-bit-mask (make-extract-mask 0 32))

(define extract-bits bitwise-and)

;;; Calculate how many bits are needed to encode INT
(define (count-bits int)
  (let lp ((int int) (c 1))
    (if (zero? int)
	c
	(lp (arithmetic-shift int -1) (+ c 1)))))

;;; rotate I COUNT bits to the left. Assumes length of 32 bits
(define (circular-shift-left i count)
  (let* ((shifted (arithmetic-shift i count))
	 (too-much (- (count-bits shifted) 32))
	 (digits (if (positive? too-much)
		     too-much
		     0)))
    (bitwise-ior 
     (extract-bits shifted first-32-bit-mask)
     (arithmetic-shift
      (extract-bits shifted (make-extract-mask 32 digits))
      -32))))

(define (calc-blocks-needed bits)
  (if (<= bits 448)
      1
      (let* ((full-blocks (+ 1 (quotient bits 512)))
	     (rest (- (* full-blocks 512) bits)))
	(if (< rest 64)
	    (+ 1 full-blocks)
	    full-blocks))))

;;; convert NUM-BYTES from BV (starting at START) into an integer
(define (bytes->block bv start num-bytes)
  (let lp ((i 0) (block 0))
    (if (= i num-bytes)
	block
	(lp (+ i 1) 
	    (bitwise-ior 
	     block
	     (arithmetic-shift (byte-vector-ref bv (+ start i))
			       (* 8 (- num-bytes (+ i 1)))))))))

;;; enough space for 64 bit length info?
(define (enough-space-for-length-info? last-block-len)
  (<= last-block-len 56))

;;; add padding to BLOCK such that BLOCK is BLOCK-LEN bits long.
(define (pad-block block block-len)
  (let* ((count (count-bits block)))
    (bitwise-ior
     (arithmetic-shift block (- block-len count))
     (arithmetic-shift #b1 (- (- block-len count) 1)))))

;;; copy 64 bits for the message length to the end of a block
(define (add-message-len len block)
  (+ block len))

(define (finish-blocks blocks maybe-last-block last-block-len)
  'ficken)

;  (if (enough-space-for-length-info? last-block-len)
  
;;; convert a byte-vector into a list of blocks (which are 512 bit
;;; integers)
(define (byte-vector->blocks bv)
  (let ((len (byte-vector-length bv)))
    (let lp ((i 0) (blocks '()))
      (cond
       ((>= (+ i 64) len)
	(let* ((last-block-len (- len i))
	       (maybe-last-block (bytes->block bv i last-block-len)))
	  (finish-blocks blocks maybe-last-block last-block-len)))
       (else
	(lp (+ i 64) (cons (bytes->block bv i 64) blocks)))))))

;;; generate a vector with masks that decompose a 512-bit block into
;;; 16 32-bit words (stored in a vector)
(define (make-split-block-vector)
  (let ((vec (make-vector 16 0)))
    (do ((i 0 (+ i 32))
	 (j 0 (+ j 1)))
	((>= i 512) vec)
      (vector-set! vec
		   j
		   (make-extract-mask i 32)))))

(define split-block-masks
  (make-split-block-vector))

;;; decompose a 512-bit block into 16 32-bit words (stored in a
;;; vector)
(define (split-block block)
  (let ((vec (make-vector 16 0)))
    (do ((i 0 (+ i 1)))
	((>= i 16) vec)
      (vector-set! 
       vec (- 15 i)
       (arithmetic-shift
	(bitwise-and (vector-ref split-block-masks i)
		     block)
	(- (* i 32)))))))

;;; extend a vector with 16 32-bit words into a vector of 80 32-bit
;;; words
(define (extend-word-blocks word-block)
  (let ((vec (make-vector 80 0)))

    (do ((i 0 (+ i 1)))
	((> i 15) (values))
      (vector-set! vec i (vector-ref word-block i)))

    (do ((i 16 (+ i 1)))
	((> i 79) vec)
      (vector-set! 
       vec i
       (circular-shift-left
	(bitwise-xor (vector-ref vec (- i 3))
		     (bitwise-xor (vector-ref vec (- i 8))
				  (bitwise-xor (vector-ref vec (- i 14))
					       (vector-ref vec (- i 16)))))
	1)))))

;;; the nonlinear functions used by SHA1
(define (nonlinear-sha1-function i x y z)
  (cond
   ((<= i 19)
    (bitwise-xor (bitwise-and x y) 
		 (bitwise-and (bitwise-not x) z)))
   ((<= i 39)
    (bitwise-xor (bitwise-xor x y) z))
   ((<= i 59)
    (bitwise-xor
     (bitwise-xor (bitwise-and x y)
		  (bitwise-and x z))
     (bitwise-and y z)))
   (else
    (bitwise-xor (bitwise-xor x y) z))))

;;; code to add numbers modulo 2^32
(define ring-size (expt 2 32))

(define (mod2+ a b)
  (modulo (+ a b) ring-size))

(define (mod+ a b c d e)
  (mod2+ (mod2+ (mod2+ (mod2+ a b) c) d) e))

;;; the SHA1 "constants"
(define (sha1-constant i)
  (cond
   ((<= i 19) #x5a827999)
   ((<= i 39) #x6ed9eba1)
   ((<= i 59) #x8f1bbcdc)
   (else #xca62c1d6)))

(define (display-state i a b c d e)
  (let ((as-hex (lambda (v)
		  (number->string v 16))))
    (display
     (format "~a\t ~a  ~a  ~a  ~a  ~a"
	     i
	     (as-hex a) (as-hex b) (as-hex c)
	     (as-hex d) (as-hex e)))
    (newline)))

;;; append five 32 bits to a 160 bit hash number
(define (append-hash h0 h1 h2 h3 h4)
  (bitwise-ior
   (bitwise-ior
    (bitwise-ior
     (bitwise-ior h4 (arithmetic-shift h3 32))
     (arithmetic-shift h2 64))
    (arithmetic-shift h1 96))
   (arithmetic-shift h0 128)))

;;; SHA1 main loop
(define (sha1-loop extended-words h0 h1 h2 h3 h4)
  (let lp ((i 0) (a h0) (b h1) (c h2) (d h3) (e h4))
    (display-state i a b c d e)
    (if (= i 80)
	(values a b c d e)
	(lp (+ i 1)
	    (mod+ (circular-shift-left a 5)
		  (nonlinear-sha1-function i b c d)
		  e
		  (vector-ref extended-words i)
		  (sha1-constant i))
	    a
	    (circular-shift-left b 30)
	    c
	    d))))

(define (calculate-sha1 blocks)
  (let lp ((blocks blocks)
	   (h0 #x67452301) (h1 #xefcdab89) (h2 #x98badcfe)
	   (h3 #x10325476) (h4 #xc3d2e1f0))
    (if (null? blocks)
	(append-hash h0 h1 h2 h3 h4)
	(let* ((block (car blocks))
	       (word-blocks (split-block block))
	       (extended-words (extend-word-blocks word-blocks)))
	  (call-with-values
	      (lambda ()
		(sha1-loop extended-words h0 h1 h2 h3 h4))
	    (lambda (a b c d e)
	      (let ((h0 (mod2+ h0 a))
		    (h1 (mod2+ h1 b))
		    (h2 (mod2+ h2 c))
		    (h3 (mod2+ h3 d))
		    (h4 (mod2+ h4 e)))
		(lp (cdr blocks) h0 h1 h2 h3 h4))))))))

