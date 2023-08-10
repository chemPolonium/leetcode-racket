#lang racket

(define/contract (min-falling-path-sum grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define (min2 l)
    (for/fold ([m1 114514] [m2 114514] [i1 0])
              ([(n i) (in-indexed l)])
      (cond [(< n m1) (values n m1 i)]
            [(< n m2) (values m1 n i1)]
            [else (values m1 m2 i1)])))
  (for/fold ([l (car grid)] #:result (apply min l))
            ([r (in-list (cdr grid))])
    (define-values (m1 m2 i1) (min2 l))
    (for/list ([(c i) (in-indexed (in-list r))])
      (if (= i i1) (+ c m2) (+ c m1)))))

; (min-falling-path-sum '((1 2 3) (4 5 6) (7 8 9)))

(min-falling-path-sum '((-73 61 43 -48 -36)
                        (3 30 27 57 10)
                        (96 -76 84 59 -15)
                        (5 -49 76 31 -7)
                        (97 91 61 -46 67)))
