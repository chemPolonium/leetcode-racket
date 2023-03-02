#lang racket

(define (print-bin num)
  (define l
    (let/cc return
      (let iter ([i 0] [n 0.5] [r num])
        (cond [(zero? r) null]
              [(= i 25) (return #f)]
              [(<= n r) (cons #\1 (iter (add1 i) (/ n 2) (- r n)))]
              [else (cons #\0 (iter (add1 i) (/ n 2) r))]))))
  (if l
      (string-append "0." (list->string l))
      "ERROR"))

(print-bin 0.625)