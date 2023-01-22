#lang racket

;;; TIME OUT

(define/contract (max-happy-groups batchSize groups)
  (-> exact-integer? (listof exact-integer?) exact-integer?)
  (define groups-mod (map (lambda (x) (remainder x batchSize)) groups))
  (define auto-happy (count zero? groups-mod))
  (define groups-positive-mod (filter positive? groups-mod))
  (define start-state
    (let ([v (make-vector (sub1 batchSize))])
      (for ([m (in-list groups-positive-mod)])
        (define i (sub1 m))
        (vector-set! v i (add1 (vector-ref v i))))
      (vector->list v)))
  (define zero-state (make-list (sub1 batchSize) 0))
  (define sum-hash (make-hash))
  (define (state-sum state)
    (hash-ref! sum-hash state
               (for/sum ([n (in-list state)]
                         [i (in-naturals 1)])
                 (* n i))))
  (define ans-hash (make-hash))
  (hash-set! ans-hash zero-state 0)
  (define (recr state)
    (hash-ref! ans-hash state
               (for/fold ([m 0])
                         ([i (in-range (sub1 batchSize))])
                 (define base-state (list-update state i sub1))
                 (cond [(ormap negative? base-state) m]
                       [else
                        (define sum-of-base (state-sum base-state))
                        (define ans-of-base-state (recr base-state))
                        (define ans
                          (if (zero? (remainder sum-of-base batchSize))
                              (add1 ans-of-base-state)
                              ans-of-base-state) )
                        (max m ans)]))))
  (+ auto-happy (recr start-state)))

(max-happy-groups 3 '(1 2 3 4 5 6))