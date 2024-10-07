#lang racket

(define/contract (dest-city paths)
  (-> (listof (listof string?)) string?)
  (set-first (set-subtract (list->set (map cadr paths)) (list->set (map car paths)))))

; (define/contract (dest-city paths)
;   (-> (listof (listof string?)) string?)
;   (define h (make-hash paths))
;   (let iter ([c (caar paths)])
;     (if (hash-has-key? h c) (iter (car (hash-ref h c))) c)))
