#lang racket

(define/contract (count-points rings)
  (-> string? exact-integer?)
  (define (intchar->int c) (- (char->integer c) 48))
  (define r (make-vector 10 #f))
  (define g (make-vector 10 #f))
  (define b (make-vector 10 #f))
  (for ([p (in-slice 2 (in-string rings))])
    (match p
      [(list #\R i) (vector-set! r (intchar->int i) #t)]
      [(list #\G i) (vector-set! g (intchar->int i) #t)]
      [(list #\B i) (vector-set! b (intchar->int i) #t)]))
  (for/fold ([c 0])
            ([r (in-vector r)]
             [g (in-vector g)]
             [b (in-vector b)])
    (if (and r g b) (add1 c) c)))

(count-points "B0B6G0R6R0R6G9")
