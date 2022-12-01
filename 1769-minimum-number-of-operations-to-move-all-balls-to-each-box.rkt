#lang racket

(define/contract (min-operations boxes)
  (-> string? (listof exact-integer?))
  (define-values (s0 r0)
    (for/fold ([s0 0]
               [r0 0])
              ([c (in-string boxes)]
               [i (in-naturals)])
      (if (equal? c #\1)
          (values (+ i s0) (add1 r0))
          (values s0 r0))))
  (define (recr s l r res)
    (cond [(null? res) null]
          [(equal? (car res) #\1)
           (cons s (recr (- (+ s l 2) r) (add1 l) (sub1 r) (cdr res)))]
          [else
           (cons s (recr (- (+ s l) r) l r (cdr res)))]))
  (recr s0 0 r0 (string->list boxes)))

(min-operations "0010")