(define-structure mysql-low (export)
  (open scheme-with-scsh
	define-record-types
	threads
	bitwise byte-vectors

	rendezvous rendezvous-channels
	srfi-11 srfi-23)
  (files mysql))
