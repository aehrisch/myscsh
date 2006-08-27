;;; enum-sets
;;; #########

(define-interface enum-sets-interface
  (export (define-enum-set-type :syntax)
          enum-set->list
          enum-set-member?
          enum-set=?
          enum-set-union
          enum-set-intersection
          enum-set-negation))

(define-interface enum-sets-internal-interface
  (export enum-set-has-type?
          enum-set?
          enum-set-type
          enum-set->integer
          integer->enum-set))

(define-structures ((enum-sets enum-sets-interface)
                    (enum-sets-internal enum-sets-internal-interface))
  (open scheme define-record-types
        finite-types
        bitwise 
        util
        signals
        external-calls)
  (optimize auto-integrate)
  (files enum-set))

;;; srfi-42-byte-vectors
;;; ####################

(define-structure srfi-42-byte-vectors
    (export (byte-vector-ec :syntax)
	    (:byte-vector :syntax))
  (open scheme srfi-42
	byte-vectors)
  (files bytevec-42))

;;; utils
;;; #####

(define-structure mysql-utils
    (export string->byte-vector
	    byte-vector->string
	    mod2+ mod5+ mod2*
	    floor-int)
  (open scheme
	srfi-42 srfi-42-byte-vectors
	ascii)
  (files util))

;;; mysql-low
;;; #########

(define-structure mysql-low (export)
  (open (modify scheme-with-scsh
		(hide select format))
	define-record-types
	threads
	finite-types enum-sets enum-sets-internal
	ascii bitwise byte-vectors

	sha1
	rendezvous rendezvous-channels
	srfi-11 srfi-23 srfi-28 srfi-42)
  (files mysql))

;;; sha1
;;; ####

(define-interface sha1-interface
  (export sha1-hash-string
	  sha1-hash-byte-vector

	  hash-value->byte-vector))

(define-structure sha1 sha1-interface
  (open scheme
	bitwise byte-vectors ascii 
	srfi-28)
  (files sha1))
