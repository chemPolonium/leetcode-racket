#lang racket

(define/contract (greatest-letter s)
  (-> string? string?)
  (define-values (u l) (partition char-upper-case? (string->list s)))
  (define valid-chars (set->list (set-intersect (list->seteq u)
                                                (list->seteq (map char-upcase l)))))
  (if (empty? valid-chars) "" (string (argmax char->integer valid-chars))))

(greatest-letter "arRAzFif")