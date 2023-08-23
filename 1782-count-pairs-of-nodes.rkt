#lang racket

(define/contract (count-pairs n edges queries)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?) (listof exact-integer?))
  (define (vector-update! vec pos updater)
    (vector-set! vec pos (updater (vector-ref vec pos))))
  (define (vector-upper-bound vec value [start 0] [end (vector-length vec)])
    (let iter ([start start]
               [end end])
      (if (= start end)
          start
          (let* ([i (quotient (+ start end) 2)]
                 [it (vector-ref vec i)])
            (if (> it value)
                (iter start i)
                (iter (add1 i) end))))))
  (define degree (make-vector n))
  (define cnt (make-hash))
  (for ([e (in-list edges)])
    (match-define (list x+1 y+1) e)
    (define x (sub1 (min x+1 y+1)))
    (define y (sub1 (max x+1 y+1)))
    (vector-update! degree x add1)
    (vector-update! degree y add1)
    (hash-update! cnt (cons x y) add1 0))
  (define arr (vector-copy degree))
  (vector-sort! arr <)
  (for/list ([bound (in-list queries)])
    (- (for/sum ([i (in-range n)])
         (define j
           (vector-upper-bound arr (- bound (vector-ref arr i)) (add1 i)))
         (- n j))
       (for/sum ([(key freq) (in-hash cnt)])
         (match-define (cons x y) key)
         (if (and (> (+ (vector-ref degree x) (vector-ref degree y)) bound)
                  (<= (+ (vector-ref degree x) (vector-ref degree y)) (+ bound freq)))
             1
             0)))))

(count-pairs 4 '((1 2) (2 4) (1 3) (2 3) (2 1)) '(2 3))
