(define-syntax byte-vector-ec
  (syntax-rules ()
    ((byte-vector-ec etc1 etc ...)
     (apply byte-vector (list-ec etc1 etc ...)))))

(define-syntax :byte-vector
  (syntax-rules (index)
    ((:byte-vector cc var arg)
     (:byte-vector cc var (index i) arg) )
    ((:byte-vector cc var (index i) arg)
     (:do cc
          (let ((vec arg) (len 0)) 
            (set! len (byte-vector-length vec)))
          ((i 0))
          (< i len)
          (let ((var (byte-vector-ref vec i))))
          #t
          ((+ i 1)) ))

    ((:byte-vector cc var (index i) arg1 arg2 arg ...)
     (:parallel cc (:byte-vector cc var arg1 arg2 arg ...) (:integers i)) )
    ((:byte-vector cc var arg1 arg2 arg ...)
     (:do cc
          (let ((vec #f)
                (len 0)
                (vecs (ec-:vector-filter (list arg1 arg2 arg ...))) ))
          ((k 0))
          (if (< k len)
              #t
              (if (null? vecs)
                  #f
                  (begin (set! vec (car vecs))
                         (set! vecs (cdr vecs))
                         (set! len (byte-vector-length vec))
                         (set! k 0)
                         #t )))
          (let ((var (byte-vector-ref vec k))))
          #t
          ((+ k 1)) ))))

(define (ec-:byte-vector-filter vecs)
  (if (null? vecs)
      '()
      (if (zero? (byte-vector-length (car vecs)))
          (ec-:byte-vector-filter (cdr vecs))
          (cons (car vecs) (ec-:byte-vector-filter (cdr vecs))) )))

