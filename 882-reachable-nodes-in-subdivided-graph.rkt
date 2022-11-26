#lang racket

(require data/heap)

(define/contract (reachable-nodes edges maxMoves n)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer? exact-integer?)

  (define ad-vec (make-vector n empty))

  (for ([e edges])
    (match e
      [(list u v nodes)
       (vector-set*! ad-vec
                     u (cons (cons v nodes) (vector-ref ad-vec u))
                     v (cons (cons u nodes) (vector-ref ad-vec v)))]))

  (define used (make-hash))
  (define visited (mutable-set))
  (define reachable 0)
  (define pq (make-heap (lambda (a b) (<= (car a) (car b)))))

  (heap-add! pq (cons 0 0))

  (define (iter)
    (when (and (positive? (heap-count pq)) (<= (car (heap-min pq)) maxMoves))
      (match (heap-min pq)
        [(cons step u)
         (heap-remove-min! pq)
         (cond [(set-member? visited u) (iter)]
               [else
                (set-add! visited u)
                (set! reachable (add1 reachable))
                (for ([v-nodes (vector-ref ad-vec u)])
                  (match v-nodes
                    [(cons v nodes)
                     (hash-set! used (cons u v) (min nodes (- maxMoves step)))
                     (when (and (<= (+ nodes step 1) maxMoves) (not (set-member? visited v)))
                       (heap-add! pq (cons (+ nodes step 1) v)))]))])])
      (iter)))

  (iter)

  (for ([e edges])
    (match e
      [(list u v nodes)
       (set! reachable
             (+ reachable (min nodes
                               (+ (hash-ref used (cons u v) 0)
                                  (hash-ref used (cons v u) 0)))))]))
  reachable)

(reachable-nodes '((0 1 10) (0 2 1) (1 2 2)) 6 3)

(reachable-nodes '((0 1 4) (1 2 6) (0 2 8) (1 3 1)) 10 4)

(reachable-nodes '((1 2 4) (1 4 5) (1 3 1) (2 3 4) (3 4 5)) 17 5)

(reachable-nodes '([2 4 2] [3 4 5] [2 3 1] [0 2 1] [0 3 5]) 14 5)