#lang racket

(define/contract (maximal-rectangle matrix)
  (-> (listof (listof char?)) exact-integer?)
  (define (largest-rectangle-area heights)
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

  (for/fold ([heights (make-list (length (car matrix)) 0)] [m 0] #:result m)
            ([row (in-list matrix)])
    (define nrow (map (lambda (a b) (if (char=? b #\0) 0 (add1 a))) heights row))
    (values nrow (max m (largest-rectangle-area nrow)))))

(maximal-rectangle '((#\1 #\0 #\1 #\0 #\0) (#\1 #\0 #\1 #\1 #\1) (#\1 #\1 #\1 #\1 #\1)))
(maximal-rectangle '((#\0)))
(maximal-rectangle '((#\1)))
