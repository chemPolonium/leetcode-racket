#lang racket

(define/contract (find-replace-string s indices sources targets)
  (-> string? (listof exact-integer?) (listof string?) (listof string?) string?)
  (define unsorted-replace-list
    (for/list ([indice (in-list indices)]
               [source (in-list sources)]
               [target (in-list targets)]
               #:when (and (<= (+ indice (string-length source)) (string-length s))
                           (string=? (substring s indice (+ indice (string-length source)))
                                     source)))
      (list* indice (+ indice (string-length source)) target)))
  (define replace-list
    (sort unsorted-replace-list < #:key car))
  (let iter ([prev-end 0] [replace-list replace-list] [substring-list empty])
    (cond [(null? replace-list)
           (apply string-append (reverse (cons (substring s prev-end) substring-list)))]
          [else
           (match-define (list* start end target) (car replace-list))
           (iter end
                 (cdr replace-list)
                 (list* target (substring s prev-end start) substring-list))])))

; (find-replace-string "abcd" '(0 2) '("a" "cd") '("eee" "ffff"))

(find-replace-string "abcde" '(2 2) '("cdef" "bc") '("f" "fe"))
