#lang racket

(require data/heap)

(define stock-price%
  (class object%
    (super-new)
    (init-field)

    (define price-log (make-hasheq))
    (define max-timestamp 0)
    (define time-heap (make-heap (λ (a b) (>= (car a) (car b)))))
    (define max-heap (make-heap (λ (a b) (>= (cdr a) (cdr b)))))
    (define min-heap (make-heap (λ (a b) (<= (cdr a) (cdr b)))))

    ; update : exact-integer? exact-integer? -> void?
    (define/public (update timestamp price)
      (hash-set! price-log timestamp price)
      (heap-add! max-heap (cons timestamp price))
      (heap-add! min-heap (cons timestamp price))
      (heap-add! time-heap (cons timestamp price)))
    ; current : -> exact-integer?
    (define/public (current)
      (define c (heap-min time-heap))
      (cond [(= (cdr c) (hash-ref price-log (car c)))
             (cdr c)]
            [else
             (heap-remove-min! time-heap)
             (current)]))
    ; maximum : -> exact-integer?
    (define/public (maximum)
      (define c (heap-min max-heap))
      (cond [(= (cdr c) (hash-ref price-log (car c)))
             (cdr c)]
            [else
             (heap-remove-min! max-heap)
             (maximum)]))
    ; minimum : -> exact-integer?
    (define/public (minimum)
      (define c (heap-min min-heap))
      (cond [(= (cdr c) (hash-ref price-log (car c)))
             (cdr c)]
            [else
             (heap-remove-min! min-heap)
             (minimum)]))))

(define obj (new stock-price%))
(send obj update 1 10)
(send obj update 2 5)
(send obj current)
(send obj maximum)
(send obj update 1 3)
(send obj maximum)
(send obj update 4 2)
(send obj minimum)
