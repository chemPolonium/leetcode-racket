#lang racket

(define (string->pairs s)
  (define (iter i c sl res)
    (cond [(null? sl) (cons (cons c i) res)]
          [(or (false? c) (equal? c (car sl))) (iter (add1 i) (car sl) (cdr sl) res)]
          [else (iter 0 #f sl (cons (cons c i) res))]))
  (let ([slist (string->list s)])
    (iter 0 #f slist empty)))

(define (expressive? spairs wpairs)
  (let ([sn (null? spairs)]
        [wn (null? wpairs)])
    (cond [(and sn wn) true]
          [(not (or sn wn))
           (and (equal? (caar spairs) (caar wpairs))
                (or (= (cdar spairs) (cdar wpairs))
                    (>= (cdar spairs) (max (cdar wpairs) 3)))
                (expressive? (cdr spairs) (cdr wpairs)))]
          [else false])))

(define/contract (expressive-words s words)
  (-> string? (listof string?) exact-integer?)
  (let* ([spairs (string->pairs s)]
         [sexpressive? (lambda (wpairs) (expressive? spairs wpairs))])
    (count sexpressive? (map string->pairs words))))

(expressive? (string->pairs "dddiiiinnssssssoooo")
             (string->pairs "ddiinnso"))

(expressive-words
 "dddiiiinnssssssoooo"
 '("dinnssoo" "ddinso" "ddiinnso" "ddiinnssoo" "ddiinso" "dinsoo" "ddiinsso" "dinssoo" "dinso"))