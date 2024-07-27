#lang racket

(define/contract (get-smallest-string s k)
  (-> string? exact-integer? string?)
  ;;; recursive version 202ms
  ; (list->string
  ;  (let recr ([sl (string->list s)] [k k])
  ;    (cond [(null? sl) null]
  ;          [(zero? k) sl]
  ;          [else
  ;           (define i (char->integer (car sl)))
  ;           (define dis1 (min (- i 97) (- 123 i)))
  ;           (if (<= dis1 k)
  ;               (cons #\a (recr (cdr sl) (- k dis1)))
  ;               (cons (integer->char (- i k)) (cdr sl)))])))
  ;;; iterative version 201ms
  (let iter ([i 0] [k k])
    (if (= i (string-length s))
        (make-string i #\a)
        (let* ([ci (char->integer (string-ref s i))]
               [dis1 (min (- ci 97) (- 123 ci))])
          (cond [(> k dis1) (iter (add1 i) (- k dis1))]
                [(= k dis1)
                 (string-append (make-string (add1 i) #\a)
                                (substring s (add1 i)))]
                [else
                 (string-append (make-string i #\a)
                                (string (integer->char (- ci k)))
                                (substring s (add1 i)))])))))

(get-smallest-string "zbbz" 1)
