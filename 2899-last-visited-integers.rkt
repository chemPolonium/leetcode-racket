#lang racket

(require data/gvector)

(define/contract (last-visited-integers words)
  (-> (listof string?) (listof exact-integer?))
  (define-values (nums)
    (filter (λ (w) (not (string=? w "prev"))) words))
  (define nums-vec
    (list->vector (map string->number nums)))
  (define a (make-gvector))
  (for/fold ([c 0] [i 0])
            ([w (in-list words)])
    (cond [(string=? w "prev")
           (define nc (add1 c))
           (define j (- i nc))
           (cond [(< j 0)
                  (gvector-add! a -1)]
                 [else
                  (gvector-add! a (vector-ref nums-vec j))])
           (values nc i)]
          [else
           (values 0 (add1 i))]))
  (gvector->list a))

(last-visited-integers '("1" "2" "prev" "prev" "prev"))
(last-visited-integers '("1" "prev" "2" "prev" "prev"))
