(define (msg . args)
  (display (apply format args)))

(define-record-type connection :connection
  (make-connection socket in-channel out-port)
  connection?
  (socket connection-socket)
  (in-channel connection-in-channel)
  (out-port connection-out-port))

(define (port->channel port)
  (let ((ch (make-channel)))
    (spawn (lambda ()
	     (let lp ()
	       (send ch (read-char port))
	       (lp))))
    ch))

(define (make-timeout-rv time)
  (let ((ch (make-channel)))
    (spawn (lambda ()
	     (sleep time)
	     (send ch 'ignore-me)))
    (receive-rv ch)))

(define (read-n-bytes-with-timeout channel count timeout)
  (apply values
	 (let ((bv (make-byte-vector count 0))
	       (timeout-rv (make-timeout-rv timeout)))
	   (let lp ((idx 0))
	     (cond
	      ((= idx count)
	       (list bv count))
	      (else
	       (select
		(wrap (receive-rv channel)
		      (lambda (c)
			(byte-vector-set!
			 bv idx (char->ascii c))
			(lp (+ idx 1))))
		(wrap timeout-rv
		      (lambda (ignored)
			(list bv count))))))))))

(define (parse-packet-header bv)
  (values
   ;; body length
   (read-24Bit-integer bv 0)
   ;; packet sequence number
   (read-8Bit-integer bv 3)))

(define (read-packet-header channel timeout)
  (let ((header-len 4))
    (call-with-values
	(lambda ()
	  (read-n-bytes-with-timeout channel header-len timeout))
      (lambda (bv count)
	(cond
	 ((= count header-len)
	  (parse-packet-header bv))
	 (else
	  (error "timeout")))))))

(define (read-packet conn timeout)
  (let ((channel (connection-in-channel conn)))
    (call-with-values
	(lambda ()
	  (read-packet-header channel timeout))
      (lambda (body-len packet-no)
	(call-with-values
	    (lambda ()
	      (read-n-bytes-with-timeout channel body-len timeout))
	  (lambda (bv read)
	    (if (not (= read body-len))
		(error "Corrupted package" read body-len))
	    bv))))))

(define (format-byte byte)
  (let ((str (number->string byte 16)))
    (if (< byte 16)
	(string-append "0" str)
	str)))

(define (print-packet packet)
  (do-ec (:byte-vector b (index i) packet)
    (begin
      (msg "~a  " (format-byte b))
      (if (zero? (remainder (+ i 1) 10))
	  (newline))))
  (newline))

(define (byte-vector->string bv)
  (string-ec (:byte-vector b bv) (ascii->char b)))

(define (write-packet conn packet)
  (let ((port (connection-out-port conn)))
    (write-string (byte-vector->string packet) port)
    ;(send-message port (byte-vector->string packet))
    (force-output port)))
 
(define (read-null-terminated-string bv start)
  (let ((len (byte-vector-length bv)))
    (let lp ((str "") (index start))
      (cond
       ((> index len)
	(error "Could not read string" bv start))
       ((zero? (byte-vector-ref bv index))
	(values str (+ index 1)))
       (else
	(lp (string-append str (string (ascii->char (byte-vector-ref bv index))))
	    (+ index 1)))))))

(define (read-fix-length-string bv start str-len)
  (let ((bv-len (byte-vector-length bv)))
    (if (> (+ start str-len) bv-len)
	(error "Could not read fix length string" bv start str-len)
	(values (string-ec (:range i start (+ start str-len))
		  (ascii->char (byte-vector-ref bv i)))
		(+ start str-len)))))

(define (make-read-integer int-len return-next-offset?)
  (lambda (bv start)
    (if (> (+ start int-len) (byte-vector-length bv))
	(error (format "Can't read integer (~a bytes) at index ~a" 
		       int-len start bv)))
    (let ((int-value
	   (sum-ec (:range i 0 int-len)
	     (arithmetic-shift (byte-vector-ref bv (+ start i))
			       (* 8 i))))) ;(+ start i))))))
      (if return-next-offset?
	  (values int-value (+ start int-len))
	  int-value))))

;; read functions ending with * return two values: the integer read and
;; the index of the field.  read functions without * just return the integer
;; read.

(define read-8Bit-integer  (make-read-integer 1 #f))
(define read-8Bit-integer* (make-read-integer 1 #t))

(define read-16Bit-integer  (make-read-integer 2 #f))
(define read-16Bit-integer* (make-read-integer 2 #t))

(define read-24Bit-integer  (make-read-integer 3 #f))
(define read-24Bit-integer* (make-read-integer 3 #t))

(define read-32Bit-integer  (make-read-integer 4 #f))
(define read-32Bit-integer* (make-read-integer 4 #t))

(define read-64Bit-integer  (make-read-integer 8 #f))
(define read-64Bit-integer* (make-read-integer 8 #t))

(define (copy-string-to-bv! bv str index)
  (do ((i index (+ i 1))
       (j 0 (+ j 0)))
      ((= j (string-length str))
       (byte-vector-set! bv i 0))
    (byte-vector-set! 
     bv i (string-ref str j))))

;;; decode server greeting

;;; taken from mysql_com.h CLIENT_* version 4.1.20

(define-enumerated-type client-option :client-option
  client-option?
  client-options
  client-option-name
  client-option-index
  (long-password found-rows long-flag connect-with-db
   no-schema compress odbc local-files ignore-space
   protocol-41 interactive ssl ignore-sigpipe transactions
   reserver secure-connection multi-statements multi-results))

(define-enum-set-type option-set :option-set
  option-set?
  make-option-set
  client-option client-option? client-options client-option-index)

(define-record-type greeting :greeting
  (make-greeting protocol-ver server-ver
		 thread-id salt capabilities
		 charset status rest-salt)
  greeting?
  (protocol-ver greeting-protocol-ver)
  (server-ver greeting-server-ver)
  (thread-id greeting-thread-id)
  (salt greeting-salt)
  (capabilities greeting-capabilities)
  (charset greeting-charset)
  (status greeting-status)
  (rest-salt greeting-rest-salt))

(define (read-server-greeting conn timeout)
  (let* ((bv (read-packet conn timeout))
	 (proto-ver (read-8Bit-integer bv 0)))
    (if (not (= 10 proto-ver))
	(error "Unsupported protocol version" proto-ver))
    (let*-values
	(((server-version index) 
	  (read-null-terminated-string bv 1))
	 ((thread-id)
	  (read-32Bit-integer bv index))
	 ((salt index)
	  (read-null-terminated-string bv (+ index 4)))
	 ((capabilities)
	  (integer->enum-set :option-set
			     (read-16Bit-integer bv index)))
	 ((charset)
	  (read-8Bit-integer bv (+ index 2)))
	 ((status)
	  (read-16Bit-integer bv (+ index 3)))
	 ((rest-salt index)
	  (read-null-terminated-string bv (+ index 5 13))))
      (make-greeting
       proto-ver server-version thread-id
       salt capabilities charset status rest-salt))))

(define (byte-vector->list bv)
  (list-ec (:range i (byte-vector-length bv))
	   (byte-vector-ref bv i)))

(define (extract-byte int index)
  (bitwise-and (arithmetic-shift int (* -8 index)) 255))

(define (make-bit-copy-function no-bytes)
  (lambda (bv int offset)
    (do-ec (:range i no-bytes) 
	   (byte-vector-set! 
	    bv (+ offset i) (extract-byte int i)))))
  
(define copy-8Bit-integer!
  (make-bit-copy-function 1))

(define copy-16Bit-integer!
  (make-bit-copy-function 2))

(define copy-24Bit-integer!
  (make-bit-copy-function 3))

(define copy-32Bit-integer!
  (make-bit-copy-function 4))

(define (no-of-elements lst)
  (sum-ec (: l lst) (length l)))

(define packet-header-length 4)

(define (copy-packet-header! packet length seq-no)
  (copy-24Bit-integer! packet length 0)
  (copy-8Bit-integer! packet seq-no 3))

;; 85 A6 03 00
(define standard-client-options
  (make-option-set (list (client-option long-password)
			 (client-option long-flag)
			 (client-option transactions)
			 (client-option protocol-41)
			 (client-option secure-connection))))

(define (copy-bytes! bv offset lst)
  (do-ec (:list b (index i) lst)
	 (byte-vector-set! bv (+ offset i) b)))

(define (make-single-message seq-no . bytes)
  (let* ((packet-length (no-of-elements bytes))
	 (packet (make-byte-vector 
		  (+ packet-header-length packet-length) 0)))
    (copy-packet-header! packet packet-length seq-no)
    (let lp ((bytes bytes) (index packet-header-length))
      (cond ((null? bytes)
	     packet)
	    (else
	     (copy-bytes! packet index (car bytes))
	     (lp (cdr bytes) (+ index (length (car bytes)))))))))
  
(define (make-null-bytes count)
  (list-ec (:range ignore count) 0))

(define (make-encode-integer-function no-bytes)
  (lambda (int)
    (list-ec (:range i no-bytes) 
	     (extract-byte int i))))

(define encode-8Bit-integer
  (make-encode-integer-function 1))

(define encode-32Bit-integer
  (make-encode-integer-function 4))

;;; we don't care about encoding...
(define (encode-string-with-null str)
  (list-ec (:string c (string-append str (string (ascii->char 0))))
	   (char->ascii c)))

(define (encode-string str)
  (list-ec (:string c str) (char->ascii c)))

(define (encode-capabilities option-set)
  (encode-32Bit-integer (enum-set->integer option-set)))

(define (encode-password password)
  (cons #x14 ;;; length of the password
	(byte-vector->list password)))

(define (encrypt-password password salt)
  (let* ((hashed-pw (sha1-hash-string password))
	 (hashed-pw-bytes (hash-value->byte-vector hashed-pw))
	 (double-hashed-pw
	  (hash-value->byte-vector
	   (sha1-hash-byte-vector
	    (hash-value->byte-vector hashed-pw))))
	 (message-plain (string-append salt 
				       (byte-vector->string double-hashed-pw)))
	 (message-hashed (hash-value->byte-vector
			  (sha1-hash-string message-plain)))
	 (len (byte-vector-length message-hashed))
	 (res (make-byte-vector len 0)))
    (byte-vector-ec 
	(:byte-vector h hashed-pw-bytes)
        (:byte-vector m message-hashed)
      (bitwise-xor h m))))

(define (make-client-auth-message seq-no
				  capabilities max-packet-size
				  charset user password salt)
  (make-single-message
   seq-no
   (encode-capabilities capabilities)
   (encode-32Bit-integer max-packet-size)
   (encode-8Bit-integer charset)
   (make-null-bytes 23)
   (encode-string-with-null user)
   (encode-password (encrypt-password password salt))))

;;; old password algorithm
;;;
;;; After sending a protocol version 4.1 authentication message the
;;; server wants us to send a message with password scrambled as in
;;; protocol version 3.23...

;;; pseudo number generator as used by MySQL

(define-record-type random-seed :random-seed
  (really-make-random-seed a b)
  random-seed?
  (a random-seed-a set-random-seed-a!)
  (b random-seed-b set-random-seed-b!))

(define random-max-number #x3FFFFFFF)

(define (make-random-seed a b)
  (really-make-random-seed (modulo a random-max-number)
			   (modulo b random-max-number)))

(define (random-number seed)
  (set-random-seed-a! 
   seed (modulo
	 (mod2+ (mod2* (random-seed-a seed) 3)
		(random-seed-b seed))
	 random-max-number))
  (set-random-seed-b! 
   seed (modulo
	 (mod2+ 33
		(mod2+ (random-seed-a seed)
		       (random-seed-b seed)))
	 random-max-number))
  (* 1.0 (/ (random-seed-a seed) random-max-number)))

;;; pre 4.1 password hashing algorithm

(define mask-32bits
  (- (arithmetic-shift 1 31) 1))

(define (hash-password password-bv)
  (let ((len (byte-vector-length password-bv)))
    (let lp ((i 0) (add 7) (nr 1345345333) (nr2 #x12345671))
      (if (= i len)
	  (values (bitwise-and nr mask-32bits) (bitwise-and nr2 mask-32bits))
	  (let ((b (byte-vector-ref password-bv i)))
	    (if (or (= b #x20) (= b #x09))  ; ignore whitespace and tab
		(lp (+ i 1) add nr nr2)
		(let* ((new-nr
			(bitwise-xor nr
				     (mod2+ (mod2* b (mod2+ add (bitwise-and nr 63)))
					    (arithmetic-shift nr 8))))
		       (new-nr2
			(mod2+ nr2 (bitwise-xor new-nr (arithmetic-shift nr2 8)))))
		  (lp (+ i 1) (mod2+ add b) new-nr new-nr2))))))))

;;; pre 4.1 scrambling algorithm

(define (scramble-password password-str message-str)
  (let*-values
      (((message-bv)  (string->byte-vector message-str))
       ((password-bv) (string->byte-vector password-str))
       ((hashed-pw-1 hashed-pw-2) (hash-password password-bv))
       ((hashed-msg-1 hashed-msg-2) (hash-password message-bv))
       ((init-1) (bitwise-xor hashed-pw-1 hashed-msg-1))
       ((init-2) (bitwise-xor hashed-pw-2 hashed-msg-2))
       ((seed) (make-random-seed init-1 init-2)))
    (let* ((bv
	    (byte-vector-ec (:byte-vector b message-bv)
	      (mod2+ (floor-int (* (random-number seed) 31.0)) 64)))
	   (extra (floor-int (* (random-number seed) 31.0))))
      (byte-vector->string
       (byte-vector-ec (:byte-vector b bv)
	 (bitwise-xor b extra))))))

(define (make-old-password-message seq-no password salt)
  (make-single-message
   seq-no
   (encode-string-with-null (scramble-password password salt))))

(define (request-for-old-password-message? bv)
  (and (= (byte-vector-length bv) 1)
       (= (byte-vector-ref bv 0) 254)))

;;; read length encoded data

(define (read-length-coded-binary packet start)
  (if (>= start (byte-vector-length packet))
      (error "Index out of range" packet start))
  (let ((b (byte-vector-ref packet start)))
    (cond
     ((<= b 250) 
      (values b (+ start 1)))
     ((= b 251) 
      (values #f (+ start 1)))
     ((= b 252)
      (values (read-16Bit-integer packet (+ start 1))
	      (+ start 3)))
     ((= b 253)
      (values (read-32Bit-integer packet (+ start 1))
	      (+ start 5)))
     ((= b 254)
      (values (read-64Bit-integer packet (+ start 1))
	      (+ start 9)))
     (else
      (error "Can't decode length encoded binary" packet start b)))))

(define (read-length-encoded-string packet start)
  (let ((len (byte-vector-ref packet start)))
    (if (>= (+ start len) (byte-vector-length packet))
	(error "Length encoded string exceeds packet" packet start len))
    (values (string-ec (:range i (+ start 1) (+ start 1 len))
	      (ascii->char (byte-vector-ref packet i)))
	    (+ start 1 len))))

;;; Given a number (i.e. the ID field), find the corresponding
;;; instance of a finite type.

(define-syntax define-finite-type-constructor
  (syntax-rules ()
    ((define-finite-type-constructor elements-vector id name)
     (define name 
       (let ((alist (map (lambda (elem)
			   (cons (id elem) elem))
			 (vector->list elements-vector))))
	 (lambda (num)
	   (cond
	    ((assoc num alist)
	     => cdr)
	    (else
	     (error "No instance of finite type" num)))))))))

;;; field types

(define-finite-type field-type :field-type
  (id)
  field-type?
  field-type-elements
  field-type-name
  field-type-index
  (id field-type-id)
  ((decimal	0)	(tiny	1)	(short	   2)
   (long	3)	(float	4)	(double    5)
   (null	6)	(timestamp 7)	(long-long 8)
   (int-24	9)	(date   10)	(time      11)
   (date-time	12)	(year	13)	(new-date  14)
   (enum	247)	(set    248)	(tiny-blob 249)
   (medium-blob 250)	(long-blob 251) (blob      252)
   (var-string  253)	(string 254)    (geometry  255)))

(define-finite-type-constructor field-type-elements 
  field-type-id make-field-type-from-number)

;;; charsets

(define-finite-type charset :charset
  (id)
  charset?
  charset-elements
  charset-name
  charset-index
  (id charset-id)
  ((big5-chinese-ci        1)   (latin1-general-cs  49)
   (latin2-czech-cs        2)   (cp1251-bin         50)
   (dec8-swedish-ci        3)   (cp1251-general-ci  51)
   (cp850-general-ci       4)   (cp1251-general-cs  52)
   (latin1-german1-ci      5)   (macroman-bin       53)
   (hp8-english-ci         6)   (macroman-ci        54)
   (koi8r-general-ci       7)   (macroman-ci-ai     55)
   (latin1-swedish-ci      8)   (macroman-cs        56)
   (latin2-general-ci      9)   (cp1256-general-ci  57)
   (swe7-swedish-ci        10)  (cp1257-bin         58)
   (ascii-general-ci       11)  (cp1257-general-ci  59)
   (ujis-japanese-ci       12)  (cp1257-ci          60)
   (sjis-japanese-ci       13)  (cp1257-cs          61)
   (cp1251-bulgarian-ci    14)  (binary             63)
   (latin1-danish-ci       15)  (armscii8-bin       64)
   (hebrew-general-ci      16)  (ascii-bin          65)
   (tis620-thai-ci         18)  (cp1250-bin         66)
   (euckr-korean-ci        19)  (cp1256-bin         67)
   (latin7-estonian-cs     20)  (cp866-bin          68)
   (latin2-hungarian-ci    21)  (dec8-bin           69)
   (koi8u-general-ci       22)  (greek-bin          70)
   (cp1251-ukrainian-ci    23)  (hebrew-bin         71)
   (gb2312-chinese-ci      24)  (hp8-bin            72)
   (greek-general-ci       25)  (keybcs2-bin        73)
   (cp1250-general-ci      26)  (koi8r-bin          74)
   (latin2-croatian-ci     27)  (koi8u-bin          75)
   (gbk-chinese-ci         28)  (latin2-bin         77)
   (cp1257-lithuanian-ci   29)  (latin5-bin         78)
   (latin5-turkish-ci      30)  (latin7-bin         79)
   (latin1-german2-ci      31)  (cp850-bin          80)
   (armscii8-general-ci    32)  (cp852-bin          81)
   (utf8-general-ci        33)  (swe7-bin           82)
   (cp1250-czech-cs        34)  (utf8-bin           83)
   (ucs2-general-ci        35)  (big5-bin           84)
   (cp866-general-ci       36)  (euckr-bin          85)
   (keybcs2-general-ci     37)  (gb2312             86)
   (macce-general-ci       38)  (gbk-bin            87)
   (macroman-general-ci    39)  (sjis-bin           88)
   (cp852-general-ci       40)  (tis620-bin         89)
   (latin7-general-ci      41)  (ucs2-bin           90)
   (latin7-general-cs      42)  (ujis-bin           91)
   (macce-bin              43)  (geostd8-general-ci 92)
   (latin1-bin             47)  (geostd8-bin        93)
   (latin1-general-ci      48)  (latin1-spanish-ci  94)))

(define-finite-type-constructor charset-elements 
  charset-id make-charset-from-number)

;;; status codes

(define-enumerated-type status-code :status-code
  status-code?
  status-codes
  status-code-name
  status-code-index
  (in-transaction auto-commit more-results more-results-exist
   no-good-index-exists db-dropped))

(define-enum-set-type status-code-set :status-code-set
  status-code-set?
  make-status-code-set
  status-code status-code? status-codes status-code-index)

;;; field flags

(define-enumerated-type field-flag :field-flag
  field-flag?
  field-flags
  field-flag-name
  field-flag-index
  (not-null primary-key unique-key multiple-key blob
   unsigned zero-fill binary enum auto-increment
   timestamp set unused-1 unused-2 partial-key group
   unique bincmp))

(define-enum-set-type field-flag-set :field-flag-set
  field-flag-set?
  make-field-flag-set
  field-flag field-flag? field-flags field-flag-index)

;;; recognize the strange packets of length 1

(define (make-control-packet-recognizer b)
  (lambda (packet)
    (and (= (byte-vector-length packet) 1)
	 (= (byte-vector-ref packet 0) b))))

(define end-of-field-list-marker?
  (make-control-packet-recognizer #xfe))

(define no-rows-marker?
  (make-control-packet-recognizer #xfb))

;;; dispatch on the result packet type

(define-record-type ok-packet :ok-packet
  (make-ok-packet affected-rows insert-id
		  server-status warning-count message)
  ok-packet?
  (affected-rows ok-packet-affected-rows)
  (insert-id ok-packet-insert-id)
  (server-status ok-packet-server-status)
  (warning-count ok-packet-warning-count)
  (message ok-packet-message))

(define (could-be-ok-packet? packet)
  (or (and (= (byte-vector-length packet) 1)
	   (= (byte-vector-ref packet 0) 1))
      (= (byte-vector-ref packet 0) #x0)))

(define (parse-ok-packet packet start)
  (if (= (byte-vector-length packet) 1)
      (make-ok-packet #f #f #f #f #f)
      (let*-values 
	  (((field-count next)   (read-8Bit-integer* packet start)) ; always #x0
	   ((affected-rows next) (read-length-coded-binary packet next))
	   ((insert-id next)     (read-length-coded-binary packet next))
	   ((server-status next) (read-16Bit-integer* packet next))
	   ((warning-count next) (read-16Bit-integer* packet next))
	   ((make-packet) (lambda (message)
			    (make-ok-packet affected-rows insert-id
					    server-status warning-count
					    message))))
	;; the message field is optional
	(if (>= next (byte-vector-length packet))
	    (make-packet #f)
	    (call-with-values
		(lambda ()
		  (read-length-encoded-string packet next))
	      (lambda (message next)
		(make-packet message)))))))

(define-record-type error-packet :error-packet
  (make-error-packet errno sql-state message)
  error-packet?
  (errno error-packet-errno)
  (sql-state error-packet-sql-state)
  (message error-packet-message))

(define (could-be-error-packet? packet)
  (= (byte-vector-ref packet 0) #xff))

(define (parse-error-packet packet start)
  ;; The first nine bytes are always fix, the rest is a string which is
  ;; not necessarily null-terminated.  Compute it's length.
  (let ((message-len (- (byte-vector-length packet) 9)))
    (let*-values
	(((field-count next) (read-8Bit-integer* packet start))	; always #xff
	 ((errno next)       (read-16Bit-integer* packet next))
	 ((sql-marker next)  (read-8Bit-integer* packet next)) ; always #\#
	 ((sql-state next)   (read-fix-length-string packet next 5))
	 ((message next)     (read-fix-length-string packet next message-len)))
      (make-error-packet errno sql-state message))))

(define-record-type eof-packet :eof-packet
  (make-eof-packet warning-count status-flags)
  eof-packet?
  (warning-count eof-packet-warning-count)
  (status-flags eof-packet-status-flags))

(define (could-be-eof-packet? packet)
  (= (byte-vector-ref packet 0) #xfe))

(define (parse-eof-packet packet start)
  (cond
   ((and (= (byte-vector-length packet) 1)
	 (= (byte-vector-ref packet 0) #xfe))
    (make-eof-packet #f #f))
   (else
    (let*-values
	(((field-count next)   (read-8Bit-integer* packet start))	; always #xfe
	 ((warning-count next) (read-16Bit-integer* packet next))
	 ((status-flags next)  (read-16Bit-integer* packet next)))
      (make-eof-packet warning-count 
		       (integer->enum-set :status-code-set status-flags))))))

(define (old-style-eof-packet? p)
  (and (eof-packet? p)
       (not (eof-packet-warning-count p)) 
       (not (eof-packet-status-flags p))))

(define-record-type result-set-header-packet :result-set-header-packet
  (make-result-set-header-packet field-count extra)
  result-set-header-packet?
  (field-count result-set-header-packet-field-count)
  (extra result-set-header-packet-extra))

(define (parse-result-set-header-packet packet start)
  (let*-values
      (((field-count next) (read-length-coded-binary packet start))
       ((extra next) (read-length-coded-binary packet next)))
    (make-result-set-header-packet field-count extra)))

(define-record-type field-packet :field-packet
  (make-field-packet catalog db table org-table
		     name org-name
		     charset length type
		     flags decimals default)
  field-packet?
  (catalog field-packet-catalog)
  (db field-packet-db)
  (table field-packet-table)
  (org-table field-packet-org-table)
  (name field-packet-name)
  (org-name field-packet-org-name)
  (charset field-packet-charset)
  (length field-packet-length)
  (type field-packet-type)
  (flags field-packet-flags)
  (decimals field-packet-decimals)
  (default field-packet-default))

(define (parse-field-packet packet start)
  (let*-values
      (((catalog next)   (read-length-encoded-string packet start))
       ((db next)        (read-length-encoded-string packet next))
       ((table next)     (read-length-encoded-string packet next))
       ((org-table next) (read-length-encoded-string packet next))
       ((name next)      (read-length-encoded-string packet next))
       ((org-name next)  (read-length-encoded-string packet next))
       ;; now there is a filling byte, so skip one
       ((next)		 (+ next 1))
       ((charset next)   (read-16Bit-integer* packet next))
       ((length next)    (read-32Bit-integer* packet next))
       ((type next)      (read-8Bit-integer* packet next))
       ((flags next)     (read-16Bit-integer* packet next))
       ((decimals next)  (read-8Bit-integer* packet next))
       ;; now there is another filling byte. skip one
       ((next)           (+ next 1))
       ((default next)   (read-length-coded-binary packet next)))
    (make-field-packet catalog db table org-table name org-name
		       (make-charset-from-number charset)
		       length 
		       (make-field-type-from-number type)
		       (integer->enum-set :field-flag-set flags)
		       decimals default)))

(define-record-type row-packet :row-packet
  (make-row-packet columns)
  row-packet?
  (columns row-packet-columns))

(define (parse-row-packet packet start)
  (let ((len (byte-vector-length packet)))
    (let lp ((index start) (columns '()))
      (if (>= index len)
	  (make-row-packet (reverse columns))
	  (call-with-values
	      (lambda ()
		(read-length-encoded-string packet index))
	    (lambda (str next)
	      (lp next (cons str columns))))))))

(define (read-row-contents-packets conn timeout)
  (let lp ((p (read-packet conn timeout)) (contents '()))
    (cond
     ((could-be-eof-packet? p)
      (values (reverse contents) (parse-eof-packet p 0)))
     ((no-rows-marker? p)
      (let ((should-be-eof-packet (read-packet conn timeout)))
	(values (parse-eof-packet should-be-eof-packet 0) '())))
     (else
      (lp (read-packet conn timeout)
	  (cons (parse-row-packet p 0) contents))))))

(define (read-field-packets conn timeout)
  (let lp ((p (read-packet conn timeout)) (fields '()))
    (cond
     ((could-be-eof-packet? p)
      (values (reverse fields) (parse-eof-packet p 0)))
     (else
      (lp (read-packet conn timeout)
	  (cons (parse-field-packet p 0) fields))))))

(define (parse-tabular-response conn timeout)
  (let*-values 
      (((fields eof-1) (read-field-packets conn timeout))
       ((rows   eof-2) (read-row-contents-packets conn timeout)))
    (list fields eof-1 rows eof-2)))

(define (read/parse-response conn timeout)
  (let ((p (read-packet conn timeout)))
    (cond
     ((could-be-ok-packet? p)
      (parse-ok-packet p 0))
     ((could-be-error-packet? p)
      (parse-error-packet p 0))
     (else
      (byte-vector-ref p 0)))))

;;; commands

(define-enumerated-type command :command
  command?
  commands
  command-name
  command-index
  (sleep quit init-db query field-list
   create-db drop-db refresh shutdown statistics
   process-info connect process-kill debug ping
   time delayed-insert change-user binlog-dump
   table-dump connect-out register-slave
   prepare execute long-data close-statement
   reset-statement set-option))

(define (encode-command-id command)
  (encode-8Bit-integer (command-index command)))

(define (make-command-message seq-no command query)
  (make-single-message
   seq-no
   (encode-command-id command)
   (encode-string query)))

(define (to-ip-address string-or-number)
  (cond
   ((string? string-or-number)
    (car (host-info:addresses (host-info string-or-number))))
   ((number? string-or-number)
    string-or-number)
   (else
    (error "Can't resolve this" string-or-number))))

(define (open-mysql-tcp-connection host port)
  (let ((sock
	 (create-socket protocol-family/internet
			socket-type/stream))
	(sock-addr
	 (internet-address->socket-address 
	  (to-ip-address host) port)))
    (connect-socket sock sock-addr)
    (make-connection
     sock
     (port->channel (socket:inport sock))
     (socket:outport sock))))

;;; test code
(define (do-login)

  (define conn 
    (open-mysql-tcp-connection "localhost" 3306))

  (define timeout 6000)

  (define greet 
    (read-server-greeting conn timeout))

  (define user "eric")
  
  (define password "abc")

  (define auth-packet 
    (make-client-auth-message
     1
     standard-client-options
     (expt 2 24)
     8
     user password
     (greeting-salt greet)))

  (write-packet conn auth-packet)

  (read-packet conn timeout)
  
  (if password
      (write-packet conn
		    (make-old-password-message 3 password (greeting-salt greet))))

  (read-packet conn timeout)

  (write-packet conn
		(make-command-message 0 (command query) "SELECT DATABASE()"))

  (let ((parsed-packet (read/parse-response conn timeout)))
    (if (ok-packet? parsed-packet)
	(parse-tabular-response conn timeout)
	(error "Query failed" parsed-packet)))

  (write-packet conn
		(make-command-message 0 (command init-db) "mysql"))

  (let ((parsed-packet (read/parse-response conn timeout)))
    (if (not (ok-packet? parsed-packet))
	(error "Init DB failed" parsed-packet)))

  (write-packet conn
		(make-command-message 0 (command query) "SELECT * FROM user"))

  (let ((parsed-packet (read/parse-response conn timeout)))
    (cond
     ((number? parsed-packet)
      (parse-tabular-response conn timeout))
     ((error-packet? parsed-packet)
      (error "Query 2 failed" parsed-packet))
     (else
      (error "Confused" parsed-packet))))

  (write-packet conn
		(make-command-message 0 (command query)
				      "CREATE TABLE tab1 (i INT, s VARCHAR(255))"))
  (let ((parsed-packet (read/parse-response conn timeout)))
    (if (not (ok-packet? parsed-packet))
	(error "Query 3 failed" parsed-packet)))

  (write-packet conn
		(make-command-message 0 (command query)
				      "INSERT INTO tab1 VALUES(42, \"Hallo Welt!\")"))

  (let ((parsed-packet (read/parse-response conn timeout)))
    (if (not (ok-packet? parsed-packet))
	(error "Query 4 failed" parsed-packet)))

  (write-packet conn
		(make-command-message 0 (command query)
				      "SELECT * FROM tab1"))
  
  (let ((parsed-packet (read/parse-response conn timeout)))
    (cond
     ((number? parsed-packet)
      (parse-tabular-response conn timeout))
     ((error-packet? parsed-packet)
      (error "Query 5 failed" parsed-packet))
     (else
      (error "Confused" parsed-packet))))

  )
