#lang racket

(define/contract (ones-minus-zeros grid)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (let* ([count-zeros (lambda (l) (count zero? l))]
         [zerosRow (map count-zeros grid)]
         [zerosCol (map count-zeros (apply map list grid))]
         [l (length (car grid))]
         [h (length grid)]
         [onesRow (map (lambda (x) (- l x)) zerosRow)]
         [onesCol (map (lambda (x) (- h x)) zerosCol)])
    (for/list ([iZerosRow zerosRow]
               [iOnesRow onesRow])
      (map (lambda (iZerosCol iOnesCol) (- (+ iOnesRow iOnesCol) iZerosRow iZerosCol))
           zerosCol onesCol))))

(ones-minus-zeros '[[0 1 1] [1 0 1] [0 0 1]])

(ones-minus-zeros '[[1 1 1] [1 1 1]])