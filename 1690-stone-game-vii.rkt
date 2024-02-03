#lang racket

(define/contract (stone-game-vii stones)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length stones))
  (define presum (make-vector (add1 n)))
  (for/fold ([s 0])
            ([(ni i) (in-indexed (in-list stones))])
    (define ns (+ s ni))
    (vector-set! presum (add1 i) ns)
    ns)
  (define (secsum i j)
    (- (vector-ref presum (add1 j))
       (vector-ref presum i)))
  (for/fold ([l (make-list n 0)]
             #:result (car l))
            ([di (in-range (sub1 n))])
    (for/list ([(s1 i) (in-indexed (in-list l))]
               [s2 (in-list (cdr l))])
      (max (- (secsum i (+ i di)) s1)
           (- (secsum (add1 i) (+ i di 1)) s2)))))

(stone-game-vii '(5 3))
(stone-game-vii '(5 3 1 4 2))
