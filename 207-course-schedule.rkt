#lang racket

(define/contract (can-finish numCourses prerequisites)
  (-> exact-integer? (listof (listof exact-integer?)) boolean?)
  (define h (make-hasheq))
  (for ([p (in-list prerequisites)])
    (hash-update! h (first p) (λ (l) (cons (second p) l)) null))
  (define (iter-search)
    (define i (hash-iterate-first h))
    (or (not i)
        (let ([k (hash-iterate-key h i)])
          (and (search-path (seteq) k)
               (iter-search)))))
  (define (search-path path-set current-key)
    (cond [(set-member? path-set current-key) #f]
          [(not (hash-has-key? h current-key))]
          [else
           (define prerequisite-list (hash-ref h current-key))
           (and (for/and ([prerequisite (in-list prerequisite-list)])
                  (search-path (set-add path-set current-key)
                               prerequisite))
                (hash-remove! h current-key))]))
  (iter-search))

(can-finish 2 '((1 0)))
(can-finish 2 '((1 0) (0 1)))
(can-finish 20 '((1 1)))
(can-finish 3 '((1 0) (1 2) (0 1)))
