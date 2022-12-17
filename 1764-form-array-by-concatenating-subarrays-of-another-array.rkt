#lang racket

(define (start-with? group nums)
  (cond [(null? group) true]
        [(null? nums) false]
        [else (and (= (car group) (car nums))
                   (start-with? (cdr group) (cdr nums)))]))

(define (choose-one group nums)
  (cond [(null? group) nums]
        [(null? nums) false]
        [(start-with? group nums)
         (drop nums (length group))]
        [else (choose-one group (cdr nums))]))

(define/contract (can-choose groups nums)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) boolean?)
  (cond [(null? groups) true]
        [(null? nums) false]
        [else (let ([l (choose-one (car groups) nums)])
                (if l
                    (can-choose (cdr groups) l)
                    false))]))

(can-choose '((1 2 3) (3 5)) '(1 2 3 4 4 3 5))