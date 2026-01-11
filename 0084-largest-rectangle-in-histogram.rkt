#lang racket

(define/contract (largest-rectangle-area heights)
  (-> (listof exact-integer?) exact-integer?)
  (define (pop st h i m)
    (let iter ([st st] [p i] [m m])
      (if (null? st)
          (values (list (cons h p)) m)
          (let ([hh (caar st)] [hi (cdar st)])
            (cond [(> hh h) (iter (cdr st) hi (max m (* hh (- i hi))))]
                  [(< hh h) (values (cons (cons h p) st) m)]
                  [else (values st m)])))))
  (let iter ([l heights] [st null] [i 0] [m 0])
    (if (null? l)
        (let-values ([(_ m) (pop st 0 i m)])
          m)
        (let-values ([(st m) (pop st (car l) i m)])
          (iter (cdr l) st (add1 i) m)))))

(largest-rectangle-area '(2 1 5 6 2 3))
(largest-rectangle-area '(2 4))
(largest-rectangle-area '(2 1 2))
(largest-rectangle-area '(4 2 0 3 2 5))
