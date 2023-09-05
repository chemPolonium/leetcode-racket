#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/contract (is-balanced root)
  (-> (or/c tree-node? #f) boolean?)
  (define (height root)
    (if (not root)
        0
        (let ([left-height (height (tree-node-left root))]
              [right-height (height (tree-node-right root))])
          (if (or (= -1 left-height)
                  (= -1 right-height)
                  (> (abs (- left-height right-height)) 1))
              -1
              (add1 (max left-height right-height))))))
  (nonnegative-integer? (height root)))

(is-balanced (tree-node 1
                        (tree-node 2
                                   (tree-node 3
                                              (make-tree-node 4)
                                              (make-tree-node 4))
                                   (make-tree-node 3))
                        (make-tree-node 2)))

(is-balanced (tree-node 1
                        #f
                        (tree-node 2
                                   #f
                                   (make-tree-node 3))))
