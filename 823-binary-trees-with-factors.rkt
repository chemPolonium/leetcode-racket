#lang racket

(define/contract (num-factored-binary-trees arr)
  (-> (listof exact-integer?) exact-integer?)
  (define sorted-arr (sort arr <))
  (define h (make-hasheq))
  (for ([i (in-list sorted-arr)])
    (hash-set! h i
               (add1 (for/sum ([j (in-list sorted-arr)] #:break (> j (sqrt i)))
                       (* (if (= j (sqrt i)) 1 2)
                          (* (hash-ref h j) (hash-ref h (/ i j) 0)))))))
  (remainder (apply + (hash-values h)) 1000000007))

(num-factored-binary-trees '(2 4 5 10))
