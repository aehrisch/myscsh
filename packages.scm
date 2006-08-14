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

;;; mysql-low
;;; #########

(define-structure mysql-low (export)
  (open (modify scheme-with-scsh
		(hide select format))
	define-record-types
	threads
	finite-types enum-sets enum-sets-internal
	ascii bitwise byte-vectors

	rendezvous rendezvous-channels
	srfi-11 srfi-23 srfi-28 srfi-42)
  (files mysql))

;;; sha1
;;; ####

(define-interface sha1-interface
  (export sha1-hash-string
	  sha1-hash-byte-vector))

(define-structure sha1 sha1-interface
  (open scheme
	bitwise byte-vectors ascii 
	srfi-28)
  (files sha1))
