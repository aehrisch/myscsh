(define (to-ip-address string-or-number)
  (cond
   ((string? string-or-number)
    (car (host-info:addresses (host-info string-or-number))))
   ((number? string-or-number)
    string-or-number)
   (else
    (error "Can't resolve this" string-or-number))))

(define (open-tcp-connection host port-no)
  (let ((sock
	 (create-socket protocol-family/internet
			socket-type/stream))
	(sock-addr
	 (internet-address->socket-address
	  (to-ip-address host) port-no)))
    (connect-socket sock sock-addr)
    (values sock (socket:inport sock) (socket:outport sock))))

	