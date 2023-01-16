#lang racket

; (define/contract (are-sentences-similar sentence1 sentence2)
;   (-> string? string? boolean?)
;   (let iter ([l1 (string-split sentence1)] [l2 (string-split sentence2)] [t 0])
;     (cond [(or (null? l1) (null? l2)) #t]
;           [(string=? (car l1) (car l2)) (iter (cdr l1) (cdr l2) t)]
;           [(zero? t) (iter (reverse l1) (reverse l2) 1)]
;           [else #f])))

(define/contract (are-sentences-similar sentence1 sentence2)
  (-> string? string? boolean?)
  (define-values (ss sl)
    (if (< (string-length sentence1) (string-length sentence2))
        (values sentence1 sentence2)
        (values sentence2 sentence1)))
  (let iter ([ls (string-split ss)] [ll (string-split sl)] [t 0])
    (cond [(null? ls) #t]
          [(string=? (car ls) (car ll)) (iter (cdr ls) (cdr ll) t)]
          [(zero? t) (iter (reverse ls) (reverse ll) 1)]
          [else #f])))

(are-sentences-similar "My name is Haley" "My Haley")

(are-sentences-similar "of" "a lot of words")

(are-sentences-similar "Eating right now" "Eating")