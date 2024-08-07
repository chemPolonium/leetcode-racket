#lang racket

(define/contract (check-if-prerequisite numCourses prerequisites queries)
  (-> exact-integer? (listof (listof exact-integer?)) (listof (listof exact-integer?)) (listof boolean?))
  (define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
  (define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
  (define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))
  (define (vec2d-update! vec m n updater) (vec2d-set! vec m n (updater (vec2d-ref vec m n))))
  (define g (make-vector numCourses empty))
  (define visited (make-vector numCourses false))
  (define ispre (make-vec2d numCourses numCourses false))
  (define (dfs cur)
    (unless (vector-ref visited cur)
      (vector-set! visited cur true)
      (for ([ne (in-list (vector-ref g cur))])
        (dfs ne)
        (vec2d-set! ispre cur ne true)
        (for ([i (in-range numCourses)])
          (vec2d-update! ispre cur i (λ (x) (or x (vec2d-ref ispre ne i))))))))
  (for ([p (in-list prerequisites)])
    (vector-set! g (first p) (cons (second p) (vector-ref g (first p)))))
  (for ([i (in-range numCourses)])
    (dfs i))
  (for/list ([query (in-list queries)])
    (vec2d-ref ispre (first query) (second query))))
