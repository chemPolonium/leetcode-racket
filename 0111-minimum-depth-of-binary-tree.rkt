#lang racket

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

; DFS
; (define/contract (min-depth root)
;   (-> (or/c tree-node? #f) exact-integer?)
;   (cond [(not root) 0]
;         [(not (tree-node-left root))
;          (add1 (min-depth (tree-node-right root)))]
;         [(not (tree-node-right root))
;          (add1 (min-depth (tree-node-left root)))]
;         [else
;          (add1 (min (min-depth (tree-node-left root))
;                     (min-depth (tree-node-right root))))]))

; BFS
(define/contract (min-depth root)
  (-> (or/c tree-node? #f) exact-integer?)
  (define (step roots leaves depth)
    (if (null? roots)
        (step leaves empty (add1 depth))
        (let* ([left (tree-node-left (first roots))]
               [leaves (if left (cons left leaves) leaves)]
               [right (tree-node-right (first roots))]
               [leaves (if right (cons right leaves) leaves)])
          (if (not (or left right))
              depth
              (step (rest roots) leaves depth)))))
  (if root
      (step (list root) empty 1)
      0))
