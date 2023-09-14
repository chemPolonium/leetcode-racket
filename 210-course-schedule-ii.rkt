#lang racket

(define/contract (find-order numCourses prerequisites)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?))
  (define edges (make-hasheq))
  (define visited (make-vector numCourses))
  (define courses (list->mutable-seteq (range numCourses)))
  (for ([p (in-list prerequisites)])
    (hash-update! edges (second p) (λ (l) (cons (first p) l)) null))
  (define (search-path u path)
    (vector-set! visited u 1)
    (define added-path
      (for/fold ([path path])
                ([v (in-list (hash-ref edges u null))]
                 #:break (not path))
        (match (vector-ref visited v)
          [0 (search-path v path)]
          [1 #f]
          [2 path])))
    (cond [(not added-path) #f]
          [else
           (vector-set! visited u 2)
           (hash-remove! edges u)
           (set-remove! courses u)
           (cons u added-path)]))
  (define (iter-search path)
    (match (hash-iterate-first edges)
      [#f path]
      [i (define k (hash-iterate-key edges i))
         (match (search-path k path)
           [#f #f]
           [path (iter-search path)])]))
  (define related-courses (iter-search empty))
  (define rest-courses (set->list courses))
  (if related-courses
      (append rest-courses related-courses)
      empty))

(find-order 2 '((1 0)))
(find-order 4 '((1 0) (2 0) (3 1) (3 2)))
(find-order 2 '((0 1) (1 0)))
