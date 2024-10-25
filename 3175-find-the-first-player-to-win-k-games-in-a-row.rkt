#lang racket

(define/contract (find-winning-player skills k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define v
    (for/vector ([(si i) (in-indexed (in-list skills))])
      (cons si i)))
  (define n (vector-length v))
  (define (simulate prev-winner-posi prev-wins)
    (define next-posi (remainder (add1 prev-winner-posi) n))
    (define still-win? (> (car (vector-ref v prev-winner-posi))
                          (car (vector-ref v next-posi))))
    (define curr-wins (if still-win? (add1 prev-wins) 1))
    (cond [(and (= curr-wins k) still-win?)
           (cdr (vector-ref v prev-winner-posi))]
          [(= curr-wins k)
           (cdr (vector-ref v next-posi))]
          [(not still-win?) (simulate next-posi 1)]
          [else
           (define temp (vector-ref v prev-winner-posi))
           (vector-set! v prev-winner-posi (vector-ref v next-posi))
           (vector-set! v next-posi temp)
           (simulate next-posi (add1 prev-wins))]))
  (if (>= k n)
      (cdr (vector-argmax car v))
      (simulate 0 0)))

(find-winning-player '(4 18 17 20 15 12 8 5) 1)
