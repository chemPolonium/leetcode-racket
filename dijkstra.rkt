#lang racket

(require data/heap)

(define (heap-consume! h)
  (let ([hm (heap-min h)])
    (heap-remove-min! h)
    hm))

(define (make-graph edges n)
  (define v (make-vector n empty))
  (for ([e edges])
    (match e
      [(list a b d)
       (vector-set! v a (cons (cons b d) (vector-ref v a)))
       (vector-set! v b (cons (cons a d) (vector-ref v b)))]))
  v)

(define (dijkstra edges start n)
  ;; edges: '((start end distance) ...)
  ;; start: start node
  ;; n: num of nodes
  (let ([graph (make-graph edges n)]
        [dists (make-vector n +inf.0)]
        [heap (make-heap (lambda (a b) (<= (car a) (car b))))])
    (heap-add! heap (cons 0 start))
    (vector-set! dists start 0)
    (define (iter visited)
      (if (zero? (heap-count heap))
          dists
          (match (heap-consume! heap)
            [(cons x-dist-from-start x)
             (cond [(set-member? visited x) (iter visited)]
                   [else
                    (for ([c (vector-ref graph x)])
                      (match c
                        [(cons y d)
                         (let ([nd (+ d x-dist-from-start)])
                           (when (< nd (vector-ref dists y))
                             (vector-set! dists y nd)
                             (heap-add! heap (cons nd y))))]))
                    (iter (set-add visited x))])])))
    (iter (set))))

(define edges '((0 1 5) (0 2 2) (0 3 2) (2 1 1)))

(define start 0)

(define n 4)

; (make-graph edges n)

(dijkstra edges start n)