#lang racket

(define/contract (sort-vowels s)
  (-> string? string?)
  (define l (sequence-fold (lambda (x y) (cons y x))
                           null
                           (sequence-map cons (in-indexed (string->list s)))))
  (define vowel-pairs
    (filter (lambda (p)
              (member (car p) (string->list "aeiouAEIOU")))
            l))
  (define positions (map cdr vowel-pairs))
  (define chars (sort (map car vowel-pairs) char>?))
  (define s-copy (string-copy s))
  (for ([c (in-list chars)]
        [p (in-list positions)])
    (string-set! s-copy p c))
  s-copy)

(sort-vowels "lEetcOde")
