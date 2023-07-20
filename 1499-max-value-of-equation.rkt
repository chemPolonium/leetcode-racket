#lang racket

(require data/heap)

(define/contract (find-max-value-of-equation points k)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer?)
  (define (l-val p) (- (car p) (cadr p)))
  (define (r-val p) (+ (car p) (cadr p)))
  (define h (make-heap (lambda (a b) (>= (r-val a) (r-val b)))))
  (define ans -200000002)
  (let iter ([ll points] [rl (cdr points)])
    (when (pair? ll)
      (define l (car ll))
      (define new-rl
        (do ([rl rl (cdr rl)])
          ((or (null? rl) (> (caar rl) (+  k (car l)))) rl)
          (heap-add! h (car rl))))
      (define heap-not-null?
        (do ()
          ((or (zero? (heap-count h)) (> (car (heap-min h)) (car l)))
           (positive? (heap-count h)))
          (heap-remove-min! h)))
      (when heap-not-null?
        (set! ans (max ans (- (r-val (heap-min h)) (l-val l)))))
      (iter (cdr ll) new-rl)))
  ans)

(find-max-value-of-equation '((1 3) (2 0) (5 10) (6 -10)) 1)
