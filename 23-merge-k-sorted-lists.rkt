#lang racket

; Definition for singly-linked list:

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(require data/heap)

(define/contract (merge-k-lists lists)
  (-> (listof (or/c list-node? #f)) (or/c list-node? #f))
  (define h (make-heap (lambda (a b) (<= (list-node-val a) (list-node-val b)))))
  (define dummy-head (make-list-node))
  (heap-add-all! h (filter identity lists))
  (do ([current-node dummy-head (list-node-next current-node)])
    ((zero? (heap-count h)) (list-node-next dummy-head))
    (set-list-node-next! current-node (heap-min h))
    (let ([current-heap-min (heap-min h)])
      (set-list-node-next! current-node current-heap-min)
      (heap-remove-min! h)
      (when (list-node-next current-heap-min)
        (heap-add! h (list-node-next current-heap-min))))))

(merge-k-lists (list (list-node 1 (list-node 4 (list-node 5 #f)))
                     (list-node 1 (list-node 3 (list-node 4 #f)))
                     (list-node 2 (list-node 6 #f))))

(merge-k-lists (list))

(merge-k-lists (list #f))
