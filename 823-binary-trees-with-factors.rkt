#lang racket

(define/contract (num-factored-binary-trees arr)
  (-> (listof exact-integer?) exact-integer?)
  (define sorted-arr (sort arr <))
  (define h (make-hasheq))
  (for ([i (in-list sorted-arr)])
    (hash-set!
     h i
     (add1
      (for/sum ([j (in-list sorted-arr)]
                #:when (zero? (remainder i j))
                #:break (> (* j j) i))
        (* (if (= (* j j) i) 1 2)
           (* (hash-ref h (quotient i j) 0) (hash-ref h j)))))))
  (remainder (apply + (hash-values h)) 1000000007))

(num-factored-binary-trees '(2 4 5 10))
