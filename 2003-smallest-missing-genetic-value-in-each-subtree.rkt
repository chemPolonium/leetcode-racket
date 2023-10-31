#lang racket

(define/contract (smallest-missing-value-subtree parents nums)
  (-> (listof exact-integer?) (listof exact-integer?) (listof exact-integer?))
  (define nums-vec (list->vector nums))
  (define n (vector-length nums-vec))
  (define parent-vec (list->vector parents))
  (define children-vec (make-vector n null))
  (for ([(p i) (sequence-tail (in-indexed (in-list parents)) 1)])
    (vector-set! children-vec p (cons i (vector-ref children-vec p))))
  (define node1
    (for/first ([(num i) (in-indexed (in-list nums))]
                #:when (= num 1))
      i))
  (define gene-set (mutable-seteq))
  (define visited (make-vector n #f))
  (define (dfs! node)
    (unless (vector-ref visited node)
      (vector-set! visited node #t)
      (set-add! gene-set (vector-ref nums-vec node))
      (map dfs! (vector-ref children-vec node))))
  (define ans-vec (make-vector n 1))
  (define (up-search! node start)
    (dfs! node)
    (define pnode
      (for/first ([i (in-naturals start)]
                  #:unless (set-member? gene-set i))
        (vector-set! ans-vec node i)
        (cons (vector-ref parent-vec node) i)))
    (unless (= -1 (car pnode))
      (up-search! (car pnode) (cdr pnode))))
  (when node1
    (up-search! node1 2))
  (vector->list ans-vec))

(smallest-missing-value-subtree '(-1 0 0 2) '(1 2 3 4))
