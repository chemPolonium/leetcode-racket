#lang racket

(define/contract (minimized-string-length s)
  (-> string? exact-integer?)
  (set-count (list->set (string->list s))))
