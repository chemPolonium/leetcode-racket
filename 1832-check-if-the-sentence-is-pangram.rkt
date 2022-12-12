#lang racket

(define (check-if-pangram sentence)
  (subset?
   (set #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
   (list->set (string->list sentence))))