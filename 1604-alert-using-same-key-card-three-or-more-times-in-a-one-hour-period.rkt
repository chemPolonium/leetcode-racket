#lang racket

(define (hhmm->minutes s)
  (+ (* 60 (string->number (substring s 0 2)))
     (string->number (substring s 3 5))))

(define (safe? l)
  (let iter ([t0 #f] [t1 #f] [l (sort (map hhmm->minutes l) >)])
    (or (empty? l)
        (let ([t (car l)])
          (and (or (not t0) (> (- t0 t) 60))
               (iter t1 t (cdr l)))))))

(define/contract (alert-names keyName keyTime)
  (-> (listof string?) (listof string?) (listof string?))
  (define h (for/fold ([h (hash)])
                      ([n (in-list keyName)]
                       [t (in-list keyTime)])
              (hash-update h n (λ (l) (cons t l)) empty)))
  (sort (for/list ([(k v) (in-hash h)]
                   #:unless (safe? v))
          k)
        string<?))

; (alert-names '["daniel" "daniel" "daniel" "luis" "luis" "luis" "luis"]
;              '["10:00" "10:40" "11:00" "09:00" "11:00" "13:00" "15:00"])

(alert-names '["john" "john" "john"]
             '["23:58" "23:59" "00:01"])