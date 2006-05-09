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
  (let ((byte (lambda (i)
		(byte-vector-ref bv i))))
    (values
     ;; body length
     (+ (byte 0) 
	(arithmetic-shift (byte 1) 8)
	(arithmetic-shift (byte 2) 16))
     ;; packet sequence number
     (byte 3))))

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

(define (read-8Bit-integer bv start)
  (if (> (+ start 1) (byte-vector-length bv))
      (error "Can't read integer" bv start))
  (byte-vector-ref bv start))

(define (read-16Bit-integer bv start)
  (if (> (+ start 2) (byte-vector-length bv))
      (error "Can't read integer" bv start))
  (let ((byte (lambda (i)
		(byte-vector-ref bv (+ start i)))))
    (+ (byte 0)
       (arithmetic-shift (byte 1) 8))))

(define (read-32Bit-integer bv start)
  (if (> (+ start 4) (byte-vector-length bv))
      (error "Can't read integer" bv start))
  (let ((byte (lambda (i)
		(byte-vector-ref bv (+ start i)))))
    (+ (byte 0)
       (arithmetic-shift (byte 1) 8)
       (arithmetic-shift (byte 2) 16)
       (arithmetic-shift (byte 3) 24))))

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
	  (read-16Bit-integer bv index))
	 ((charset)
	  (read-8Bit-integer bv (+ index 2)))
	 ((status)
	  (read-16Bit-integer bv (+ index 3)))
	 ((rest-salt index)
	  (read-null-terminated-string bv (+ index 5 13))))
      (make-greeting
       proto-ver server-version thread-id
       salt capabilities charset status rest-salt))))

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
