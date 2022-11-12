#lang racket

; (define (make-sort-hash order)
;   (define (iter h s i)
;     (if (null? s)
;         h
;         (iter (hash-set h (car s) i)
;               (cdr s)
;               (add1 i))))
;   (iter (hash)
;         (string->list order)
;         0))

; (define (less-than-order order-hash x y)
;   (< (hash-ref order-hash x 0)
;      (hash-ref order-hash y 0)))

; (define/contract (custom-sort-string order s)
;   (-> string? string? string?)
;   (let ([s-list (string->list s)]
;         [order-hash (make-sort-hash order)])
;     (let ([s-sorted-list (sort s-list
;                                (lambda (x y)
;                                  (less-than-order order-hash x y)))])
;       (list->string s-sorted-list))))

(define/contract (custom-sort-string order s)
  (-> string? string? string?)
  (let* ([order-hash (make-hash (map cons (string->list order) (range (string-length order))))]
         [less (lambda (x y) (< (hash-ref order-hash x 0) (hash-ref order-hash y 0)))])
      (list->string (sort (string->list s) less))))

(custom-sort-string "hwokrzpb" "hbwqpozrkp")

(custom-sort-string "exv" "xwvee")