#lang racket

(define/contract (second-highest s)
  (-> string? exact-integer?)
  (for/fold ([m -1] [s -1] #:result s)
            ([i (filter-map (lambda (c)
                              (and (char-numeric? c)
                                   (- (char->integer c) 48)))
                            (string->list s))])
    (cond [(> i m) (values i m)]
          [(and (< i m) (> i s)) (values m i)]
          [else (values m s)])))

(second-highest "dfa12321afd")
(second-highest "abc1111")
(second-highest "a")
(second-highest "1")
(second-highest "0")
(second-highest "sjhtz8344")
