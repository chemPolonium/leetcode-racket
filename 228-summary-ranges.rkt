#lang racket

(define/contract (summary-ranges nums)
  (-> (listof exact-integer?) (listof string?))
  (define (extract-single nums start prev)
    (if (or (null? nums) (< 1 (- (car nums) prev)))
        (values nums
                (if (= start prev)
                    (number->string start)
                    (string-append (number->string start)
                                   "->"
                                   (number->string prev))))
        (extract-single (cdr nums) start (car nums))))
  (let loop ([nums nums])
    (cond [(null? nums) null]
          [else
           (define-values (nums-s is)
             (extract-single (cdr nums) (car nums) (car nums)))
           (cons is (loop nums-s))])))

(summary-ranges '(0 1 2 4 5 7))
