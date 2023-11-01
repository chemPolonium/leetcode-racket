#lang racket

(require data/queue)

(define/contract (maximum-invitations favorite)
  (-> (listof exact-integer?) exact-integer?)
  (define favorite-vec (list->vector favorite))
  (define n (vector-length favorite-vec))
  (define indeg (make-vector n))
  (for ([favorite-i (in-list favorite)])
    (vector-set! indeg favorite-i (add1 (vector-ref indeg favorite-i))))
  (define used (make-vector n #f))
  (define f (make-vector n 1))
  (define q (make-queue))
  (for ([i (in-range n)]
        #:when (zero? (vector-ref indeg i)))
    (enqueue! q i))
  (let loop ()
    (unless (queue-empty? q)
      (define u (dequeue! q))
      (vector-set! used u #t)
      (define v (vector-ref favorite-vec u))
      (vector-set! f v (max (vector-ref f v) (add1 (vector-ref f u))))
      (vector-set! indeg v (sub1 (vector-ref indeg v)))
      (when (zero? (vector-ref indeg v))
        (enqueue! q v))
      (loop)))
  (for/fold ([ring 0]
             [total 0]
             #:result (max ring total))
            ([i (in-range n)]
             #:unless (vector-ref used i))
    (define j (vector-ref favorite-vec i))
    (cond [(= i (vector-ref favorite-vec j))
           (vector-set! used i #t)
           (vector-set! used j #t)
           (values ring (+ total (vector-ref f i) (vector-ref f j)))]
          [else
           (define cnt
             (let loop ([u i] [cnt 0])
               (let ([cnt (add1 cnt)]
                     [u (vector-ref favorite-vec u)])
                 (vector-set! used u #t)
                 (if (= u i)
                     cnt
                     (loop u cnt)))))
           (values (max ring cnt) total)])))

(maximum-invitations '(2 2 1 2))
