#lang racket

(define/contract (largest-merge word1 word2)
  (-> string? string? string?)
  (define (bigger? l1 l2)
    (cond [(null? l1) false]
          [(null? l2) true]
          [else
           (let ([i1 (char->integer (car l1))]
                 [i2 (char->integer (car l2))])
             (cond [(> i1 i2) true]
                   [(< i1 i2) false]
                   [else (bigger? (cdr l1) (cdr l2))]))]))
  (list->string
   (let recr ([l1 (string->list word1)]
              [l2 (string->list word2)])
     (cond [(null? l1) l2]
           [(null? l2) l1]
           [(bigger? l1 l2)
            (cons (car l1) (recr (cdr l1) l2))]
           [else
            (cons (car l2) (recr l1 (cdr l2)))]))))