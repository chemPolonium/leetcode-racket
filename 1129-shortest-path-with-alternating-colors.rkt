#lang racket

(define (make-graph edges n)
  (define v (make-vector n empty))
  (for ([e edges])
    (match e
      [(list a b)
       (vector-set! v a (cons b (vector-ref v a)))]))
  v)

(define (min-positive a b)
  (if (and (positive? a) (positive? b))
      (min a b)
      (max a b)))

(define/contract (shortest-alternating-paths n redEdges blueEdges)
  (-> exact-integer? (listof (listof exact-integer?)) (listof (listof exact-integer?)) (listof exact-integer?))
  (define r-dis (make-vector n -1))
  (define b-dis (make-vector n -1))
  (define r-graph (make-graph redEdges n))
  (define b-graph (make-graph blueEdges n))
  (vector-set! r-dis 0 0)
  (vector-set! b-dis 0 0)
  (define (reach! reach-vec graph-vec dis-vec dis)
    (for*/list ([c (in-list reach-vec)]
                [r (in-list (vector-ref graph-vec c))]
                #:when (negative? (vector-ref dis-vec r)))
      (vector-set! dis-vec r dis)
      r))
  (let iter ([red-reach '(0)] [blue-reach '(0)] [dis 1])
    (unless (and (empty? red-reach) (empty? blue-reach))
      (iter (reach! blue-reach r-graph r-dis dis)
            (reach! red-reach b-graph b-dis dis)
            (add1 dis))))
  (vector->list (vector-map min-positive r-dis b-dis)))

; (shortest-alternating-paths 3 '((0 1) (1 2)) '())
(shortest-alternating-paths 5 '[[3 2] [4 1] [1 4] [2 4]] '[[2 3] [0 4] [4 3] [4 4] [4 0] [1 0]])
