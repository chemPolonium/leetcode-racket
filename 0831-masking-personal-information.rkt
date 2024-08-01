#lang racket

(define/contract (mask-pii s)
  (-> string? string?)
  (define (email-mask s)
    (match (string-split s "@")
      [(list name site)
       (string-append (string (char-downcase (string-ref name 0)))
                    "*****"
                    (string (char-downcase (string-ref name (sub1 (string-length name)))))
                    "@"
                    (string-downcase site))]))
  (define (phone-mask s)
    (define last-four (list->string (take-right (filter char-numeric? (string->list s)) 4)))
    (match (count char-numeric? (string->list s))
      [10 (string-append "***-***-" last-four)]
      [11 (string-append "+*-***-***-" last-four)]
      [12 (string-append "+**-***-***-" last-four)]
      [13 (string-append "+***-***-***-" last-four)]))
  (if (string-contains? s "@")
      (email-mask s)
      (phone-mask s)))