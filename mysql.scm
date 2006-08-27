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

(define (make-read-integer int-len)
  (lambda (bv start)
    (if (> (+ start int-len) (byte-vector-length bv))
	(error (format "Can't read integer (~a bytes) at index ~a" 
		       int-len start bv)))
    (sum-ec (:range i 0 int-len)
      (arithmetic-shift (byte-vector-ref bv (+ start i))
			(* 8 (+ start i))))))

(define read-8Bit-integer (make-read-integer 1))

(define read-16Bit-integer (make-read-integer 2))

(define read-24Bit-integer (make-read-integer 3))

(define read-32Bit-integer (make-read-integer 4))

(define read-64Bit-integer (make-read-integer 8))

(define (integer->octets int bytes)
  (let lp ((res '()) (shift (* 8 (- bytes 1))))
    (display shift) (newline)
    (cond
     ((< shift 0)
      res)
     (else
      (lp (cons (bitwise-and 
		 (arithmetic-shift int (- shift))
		 255)
		res)
	  (- shift 8))))))

(define (copy-integer-to-bv! bv int index bytes)
  (do ((i index (+ i 1))
       (octets (integer->octets int bytes) (cdr octets)))
      ((null? octets) (values))
      (byte-vector-set! bv i (car octets))))

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
			 (client-option interactive)
			 (client-option local-files)
			 (client-option protocol-41)
			 (client-option secure-connection)
			 (client-option multi-statements)
			 (client-option multi-results))))

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
(define (encode-string str)
  (list-ec (:string c (string-append str (string (ascii->char 0))))
	   (char->ascii c)))

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
   (encode-string user)
   (encode-password (encrypt-password password salt))))

;;; decoding result messages

(define (simple-ok-response? bv)
  (and (= (byte-vector-length bv) 1)
       (= (byte-vector-ref bv 0) 254)))

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

  (define greet 
    (read-server-greeting conn 60000))

  (define auth-packet 
    (make-client-auth-message
     1
     standard-client-options
     (expt 2 24)
     8
     "eric" "abc"
     (greeting-salt greet)))

  (write-packet conn auth-packet)
  
  (simple-ok-response? (read-packet conn 6000)))
