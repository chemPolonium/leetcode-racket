#lang racket

(define (hash-beauty h)
  (let ([l (hash-values h)])
    (if (null? l)
        0
        (- (apply max l) (apply min l)))))

(define (beauty-sum s)
  (let iter ([l (string->list s)])
    (if (null? l)
        0
        (+ (let tail-sum ([h (hash)] [l l])
             (if (null? l)
                 (hash-beauty h)
                 (+ (hash-beauty h)
                    (tail-sum (hash-update h (car l) add1 0)
                              (cdr l)))))
           (iter (cdr l))))))

(beauty-sum "aabcb")

(beauty-sum "a")