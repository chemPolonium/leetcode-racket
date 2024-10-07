#lang racket

(define/contract (min-reorder n connections)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (define h (make-hash))
  (for ([c (in-list connections)])
    (match-define (list a b) c)
    (hash-update! h a (lambda (x) (cons b x)) null)
    (hash-update! h b (lambda (x) (cons a x)) null))
  (define s (list->set connections))
  (define (f a b)
    (for/sum ([c (in-list (hash-ref h b))]
              #:unless (= c a))
      (+ (f b c) (if (set-member? s (list c b)) 0 1))))
  (f -1 0))
