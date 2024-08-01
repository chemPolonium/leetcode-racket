#lang racket

(define freq-stack%
  (class object%
    (super-new)
    (init-field [stack (make-hash)]
                [freq (make-hash)]
                [m 0])

    ; push : exact-integer? -> void?
    (define/public (push val)
      (let ([n-freq (add1 (hash-ref freq val 0))])
        (hash-set! freq val n-freq)
        (hash-update! stack n-freq (lambda (x) (cons val x)) empty)
        (set! m (max m n-freq))))

    ; pop : -> exact-integer?
    (define/public (pop)
      (let ([i (car (hash-ref stack m))])
        (hash-update! stack m cdr)
        (hash-update! freq i sub1)
        (when (null? (hash-ref stack m))
          (set! m (sub1 m)))
        i))))

;; Your freq-stack% object will be instantiated and called as such:
;; (define obj (new freq-stack%))
;; (send obj push val)
;; (define param_2 (send obj pop))

(define obj (new freq-stack%))

(send obj push 5)
(send obj push 7)
(send obj push 5)
(send obj push 7)
(send obj push 4)
(send obj push 5)
(send obj pop)
(send obj pop)
(send obj pop)
(send obj pop)
