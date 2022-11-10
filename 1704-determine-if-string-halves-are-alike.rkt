#lang racket

(define/contract (halves-are-alike s)
  (-> string? boolean?)
  (define vowel-set (set #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
  (define (vowel-count clist)
    (count (lambda (x) (set-member? vowel-set x)) clist))
  (let ([l (/ (string-length s) 2)])
    (let ([front (string->list (substring s 0 l))]
          [rear (string->list (substring s l))])
      (= (vowel-count front) (vowel-count rear)))))

(halves-are-alike "book")