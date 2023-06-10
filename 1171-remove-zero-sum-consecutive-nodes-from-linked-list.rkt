#lang racket

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define/contract (remove-zero-sum-sublists head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  (define dummy-head (list-node 0 head))
  (let iter ([cleared #t] [c-prefix-sum 0] [h (make-hash)])
    (do ([p dummy-head (list-node-next p)]) ((not p))
      (set! c-prefix-sum (+ c-prefix-sum (list-node-val p)))
      (if (hash-has-key? h c-prefix-sum)
          (begin
            (set-list-node-next! (hash-ref h c-prefix-sum) (list-node-next p))
            (set! cleared #f))
          (hash-set! h c-prefix-sum p)))
    (if cleared
        (list-node-next dummy-head)
        (iter #t 0 (make-hash)))))

(remove-zero-sum-sublists
 (list-node 1 (list-node 2 (list-node -3 (list-node 3 (list-node 1 #f))))))