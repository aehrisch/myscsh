;;; Copyright (c) 2006 by Eric Knauel. See file COPYING.

(define (write-string str port)
  (do-ec (:string c str)
    (write-char c port)))

(define (open-tcp-connection host port-no)
  (call-with-values
      (lambda ()
	(socket-client host port-no))
    (lambda (in-port out-port)
      (values #f in-port out-port))))

