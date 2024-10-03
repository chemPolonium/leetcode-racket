#lang racket

(define/contract (min-cost maxTime edges passingFees)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?) exact-integer?)
  (define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
  (define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
  (define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))
  (define passing-fees-vec (list->vector passingFees))
  (define n (vector-length passing-fees-vec))
  (define fee-vec (make-vec2d (add1 maxTime) n 1000001))
  (vec2d-set! fee-vec 0 0 (vector-ref passing-fees-vec 0))
  (for* ([t (in-inclusive-range 1 maxTime)]
         [e (in-list edges)]
         #:do [(match-define (list i j cost) e)]
         #:when (<= cost t))
    (vec2d-set! fee-vec t i
                (min (vec2d-ref fee-vec t i)
                     (+ (vec2d-ref fee-vec (- t cost) j) (vector-ref passing-fees-vec i))))
    (vec2d-set! fee-vec t j
                (min (vec2d-ref fee-vec t j)
                     (+ (vec2d-ref fee-vec (- t cost) i) (vector-ref passing-fees-vec j)))))
  (for/fold ([ans 1000001]
             #:result (if (>= ans 1000001) -1 ans))
            ([t (in-inclusive-range 1 maxTime)])
    (min ans (vec2d-ref fee-vec t (sub1 n)))))
