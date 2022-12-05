#lang racket

(define/contract (num-different-integers word)
  (-> string? exact-integer?)
  (set-count (list->set (map string->number (string-split word #rx"[a-z]+")))))

(num-different-integers "a123bc34d8ef34")