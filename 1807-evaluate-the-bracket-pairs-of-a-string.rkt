#lang racket

(define/contract (evaluate s knowledge)
  (-> string? (listof (listof string?)) string?)
  (define h (for/hash ([k (in-list knowledge)])
              (define key (first k))
              (define val (second k))
              (values (string-append "(" key ")") val)))
  (regexp-replace* #rx"\\([a-z]+\\)" s (lambda (s) (hash-ref h s "?"))))

(evaluate "(name)is(age)yearsold" '(("name" "bob") ("age" "two")))