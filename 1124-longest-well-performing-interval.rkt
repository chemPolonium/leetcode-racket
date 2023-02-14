#lang racket

(require data/splay-tree)

(define/contract (longest-wpi hours)
  (-> (listof exact-integer?) exact-integer?)
  (define l-tree (make-adjustable-splay-tree))
  (define r-tree (make-adjustable-splay-tree))
  (splay-tree-set! l-tree 0 0)
  (splay-tree-set! r-tree 0 0)
  (for/fold ([s 0])
            ([(h i) (in-indexed (in-list hours))])
    (define new-s (+ s (if (> h 8) 1 -1)))
    (define i1 (add1 i))
    (unless (splay-tree-ref l-tree new-s #f)
      (splay-tree-set! l-tree new-s i1))
    (splay-tree-set! r-tree new-s i1)
    new-s)
  (let iter ([l-iter (splay-tree-iterate-first l-tree)]
             [r-iter (splay-tree-iterate-next r-tree (splay-tree-iterate-first r-tree))]
             [l-min 114514]
             [wpi-max 0])
    (cond [r-iter
           (define l-iter-value (splay-tree-iterate-value l-tree l-iter))
           (define r-iter-value (splay-tree-iterate-value r-tree r-iter))
           (define new-l-min (min l-iter-value l-min))
           (define new-wpi (- r-iter-value new-l-min))
           (iter (splay-tree-iterate-next l-tree l-iter)
                 (splay-tree-iterate-next r-tree r-iter)
                 new-l-min
                 (max wpi-max new-wpi))]
          [else wpi-max])))

(longest-wpi '(9 9 6 0 6 6 9))
(longest-wpi '(6 6 6))