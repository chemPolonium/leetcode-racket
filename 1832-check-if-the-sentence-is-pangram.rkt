#lang racket

(define (check-if-pangram sentence)
  (= 26 (set-count (list->set (string->list sentence)))))