#lang racket

(define/contract (sort-by-bits arr)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define (count-bits x)
    (let* ([x (+ (bitwise-and 21845 x) (arithmetic-shift (bitwise-and x 43690) -1))]
           [x (+ (bitwise-and 13107 x) (arithmetic-shift (bitwise-and x 52428) -2))]
           [x (+ (bitwise-and 3855 x) (arithmetic-shift (bitwise-and x 61680) -4))]
           [x (+ (bitwise-and 255 x) (arithmetic-shift (bitwise-and x 65280) -8))])
      x))
  (define l (map (lambda (x) (cons (count-bits x) x)) arr))
  (map cdr (sort l (lambda (a b) (if (= (car a) (car b)) (< (cdr a) (cdr b)) (< (car a) (car b)))))))
