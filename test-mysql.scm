;;; Copyright (c) 2006 by Eric Knauel. See file COPYING.

(define (basic-test)

  (let* ((conn  (open-mysql-tcp-connection "localhost" 3306))
	 (greet (read-server-greeting conn))
	 (user  "eric")
	 (password "abc"))

    (write-packet conn
		  (make-client-auth-message
		   1
		   standard-client-options
		   (expt 2 24)
		   8
		   user password
		   (greeting-salt greet)))

    (read-packet conn)
  
    (if password
	(write-packet conn
		      (make-old-password-message 3 password (greeting-salt greet))))

    (read-packet conn)

    (write-packet conn
		  (make-command-message 0 (command query) "SELECT DATABASE()"))

    (let ((parsed-packet (read/parse-response conn)))
      (if (ok-packet? parsed-packet)
	  (parse-tabular-response conn)
	  (error "Query failed" parsed-packet)))

    (write-packet conn
		  (make-command-message 0 (command init-db) "mysql"))

    (let ((parsed-packet (read/parse-response conn)))
      (if (not (ok-packet? parsed-packet))
	  (error "Init DB failed" parsed-packet)))

    (write-packet conn
		  (make-command-message 0 (command query) "SELECT * FROM user"))

    (let ((parsed-packet (read/parse-response conn)))
      (cond
       ((number? parsed-packet)
	(parse-tabular-response conn))
       ((error-packet? parsed-packet)
	(error "Query 2 failed" parsed-packet))
       (else
	(error "Confused" parsed-packet))))

;  (write-packet conn
;		(make-command-message 0 (command query)
;				      "CREATE TABLE tab1 (i INT, s VARCHAR(255))"))
;  (let ((parsed-packet (read/parse-response conn)))
;    (if (not (ok-packet? parsed-packet))
;	(error "Query 3 failed" parsed-packet)))

    (write-packet conn
		  (make-command-message 0 (command query)
					"INSERT INTO tab1 VALUES(42, \"Hallo Welt!\")"))

    (let ((parsed-packet (read/parse-response conn)))
      (if (not (ok-packet? parsed-packet))
	  (error "Query 4 failed" parsed-packet)))

    (write-packet conn
		  (make-command-message 0 (command query)
					"SELECT * FROM tab1"))
  
    (let ((parsed-packet (read/parse-response conn)))
      (cond
       ((number? parsed-packet)
	(parse-tabular-response conn))
       ((error-packet? parsed-packet)
	(error "Query 5 failed" parsed-packet))
       (else
	(error "Confused" parsed-packet))))

    ))
