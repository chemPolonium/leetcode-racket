#lang racket

; (define (min-steps s t)
;   (define (count-char-hash s)
;     (for/fold ([h (hasheq)])
;               ([c (in-string s)])
;       (hash-update h c add1 0)))
;   (define s-count-char-hash (count-char-hash s))
;   (define t-count-char-hash (count-char-hash t))
;   (define dual-steps
;     (for/sum ([c (sequence-map integer->char (in-range 97 123))])
;       (abs (- (hash-ref t-count-char-hash c 0)
;               (hash-ref s-count-char-hash c 0)))))
;   (/ dual-steps 2))

(define (min-steps s t)
  (define h (make-hasheq))
  (for ([c (in-string s)])
    (hash-update! h c add1 0))
  (for ([c (in-string t)])
    (hash-update! h c sub1 0))
  (/ (apply + (map abs (hash-values h))) 2))
