#lang racket

; dp version
; (define/contract (min-capability nums k)
;   (-> (listof exact-integer?) exact-integer? exact-integer?)
;   (define (rob-1 n with-prev without-prev)
;     (define with-this
;       (cons n (map (λ (o) (if (zero? o) 0 (max n o)))
;                    (take without-prev (sub1 k)))))
;     (define without-this
;       (map (λ (w o) (if (zero? o) w (min w o)))
;            with-prev without-prev))
;     (values with-this without-this))
;   (for/fold ([with-prev (make-list k 0)]
;              [without-prev (make-list k 0)]
;              #:result ((λ (w o) (if (zero? o) w (min w o)))
;                        (last with-prev)
;                        (last without-prev)))
;             ([n (in-list nums)])
;     (rob-1 n with-prev without-prev)))

(define/contract (min-capability nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define (rob-1 n)
    (let iter ([nums nums] [p? #f] [c 0])
      (cond [(null? nums) c]
            [(or p? (> (car nums) n)) (iter (cdr nums) #f c)]
            [else (iter (cdr nums) #t (add1 c))])))
  (define (search lo hi)
    (define mid
      (let-values ([(q r) (quotient/remainder (+ lo hi) 2)])
        (+ q r)))
    (cond [(= mid hi) mid]
          [(< (rob-1 mid) k) (search mid hi)]
          [else (search lo mid)]))
  (search (sub1 (apply min nums)) (apply max nums)))

(min-capability '(2 3 5 9) 2)
; 5

(min-capability '(2 7 9 3 1) 2)
; 2

(min-capability '(2 7 9 3 1) 3)
; dp out
; with-this  without-this
; ()         ()
; (2)        ()
; (7)        (2)
; (9 9)      (2)
; (3 3)      (2 9)
; (1 2 9)    (2 3)

; (0 0 0)    (0 0 0)
; (2 0 0)    (0 0 0)
; (7 0 0)    (2 0 0)
; (9 9 0)    (2 0 0)
; (3 3 0)    (2 9 0)
; (1 2 9)    (2 3 0)
