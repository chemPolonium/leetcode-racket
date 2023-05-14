#lang racket

(define frequency-tracker%
  (class object%
    (super-new)

    (init-field)

    (define number-freq-hash (make-hasheq))
    (define freq-freq-hash (make-hasheq))

    ; add : exact-integer? -> void?
    (define/public (add number)
      (cond [(hash-has-key? number-freq-hash number)
             (define freq (hash-ref number-freq-hash number))
             (when (positive-integer? freq)
               (hash-update! freq-freq-hash freq sub1))
             (hash-update! freq-freq-hash (add1 freq) add1 0)
             (hash-set! number-freq-hash number (add1 freq))]
            [else
             (hash-set! number-freq-hash number 1)
             (hash-update! freq-freq-hash 1 add1 0)]))
    ; delete-one : exact-integer? -> void?
    (define/public (delete-one number)
      (when (positive-integer? (hash-ref number-freq-hash number 0))
        (let ([freq (hash-ref number-freq-hash number)])
          (hash-set! number-freq-hash number (sub1 freq))
          (hash-update! freq-freq-hash freq sub1)
          (when (> freq 1)
            (hash-update! freq-freq-hash (sub1 freq) add1)))))
    ; has-frequency : exact-integer? -> boolean?
    (define/public (has-frequency frequency)
      (positive-integer? (hash-ref freq-freq-hash frequency 0)))))

;; Your frequency-tracker% object will be instantiated and called as such:
;; (define obj (new frequency-tracker%))
;; (send obj add number)
;; (send obj delete-one number)
;; (define param_3 (send obj has-frequency frequency))