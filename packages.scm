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

;;; mysql-connection
;;; ################

(define-interface mysql-connection-interface
  (export open-tcp-connection
	  write-string
	  force-output))

;;; The mysql-connection module for the scsh API

(define-structure mysql-connection-scsh mysql-connection-interface
  (open scheme-with-scsh
	srfi-23)
  (files net-scsh))

;;; The mysql-connection module for the Scheme 48 API
;;;
;;; Comment out the definition for mysql-connection-scsh if you are
;;; using Scheme 48 since there isn't a scheme-with-scsh module in
;;; Scheme 48.  And vice versa: scsh doesn't have i/o nor sockets.

(define-structure mysql-connection-s48 mysql-connection-interface
  (open scheme 
	srfi-42
	i/o sockets)
  (files net-s48))

(define mysql-connection mysql-connection-s48)

;;; mysql-low
;;; #########

(define-structure mysql-low (export)
  (open scheme
	mysql-connection
	define-record-types
	finite-types enum-sets enum-sets-internal
	ascii bitwise byte-vectors

	mysql-utils sha1
	srfi-42 srfi-42-byte-vectors
	srfi-11 srfi-23 srfi-28)
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
	mysql-utils
	srfi-28)
  (files sha1))
