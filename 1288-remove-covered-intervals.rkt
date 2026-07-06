#lang racket

(define/contract (remove-covered-intervals intervals)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let iter ([l (sort intervals
                      (lambda (a b)
                        (if (= (car a) (car b))
                            (> (cadr a) (cadr b))
                            (< (car a) (car b)))))]
             [r 0]
             [ans 0])
    (cond [(null? l) ans]
          [(<= (cadar l) r) (iter (cdr l) r ans)]
          [else (iter (cdr l) (cadar l) (add1 ans))])))
