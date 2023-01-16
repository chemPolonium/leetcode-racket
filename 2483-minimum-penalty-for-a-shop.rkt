#lang racket

(define/contract (best-closing-time customers)
  (-> string? exact-integer?)
  (for/fold ([p 0]
             [bp 0]
             [bt 0]
             #:result bt)
            ([(c t) (in-indexed customers)])
    (let ([np (+ p (if (equal? #\Y c) -1 1))])
      (if (< np bp)
          (values np np (add1 t))
          (values np bp bt)))))

(best-closing-time "YYNY")
(best-closing-time "NNNNN")
(best-closing-time "YYYY")