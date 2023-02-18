#lang racket

(require data/heap)

(define/contract (max-average-ratio classes extraStudents)
  (-> (listof (listof exact-integer?)) exact-integer? flonum?)
  (struct clas (pass total gain))
  (define (make-clas pass total)
    (clas pass total (/ (- total pass) (* total (+ 1. total)))))
  (define h (make-heap (λ (a b) (>= (clas-gain a) (clas-gain b)))))
  (for ([c (in-list classes)])
    (heap-add! h (make-clas (first c) (second c))))
  (for ([_ (in-range extraStudents)])
    (define c1 (heap-min h))
    (heap-remove-min! h)
    (heap-add! h (make-clas (add1 (clas-pass c1)) (add1 (clas-total c1)))))
  (define n (+ .0 (heap-count h)))
  (for/sum ([c (in-heap/consume! h)])
    (/ (clas-pass c) (clas-total c) n)))