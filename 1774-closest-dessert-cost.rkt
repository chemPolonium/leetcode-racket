#lang racket

(define (better a b)
  (if (or (and (negative? a) (< (abs a) (abs b)))
          (and (>= a 0) (<= a (abs b))))
      a
      b))

(define (i-closest-res res toppingCosts)
  (let ([s (for/fold ([s (set res)])
                     ([t (in-list toppingCosts)])
             (for/fold ([s1 s])
                       ([r (in-set s)])
               (cond [(positive? (- r t)) (set-add (set-add s1 (- r t)) (- r t t))]
                     [(positive? r) (set-add s1 (- r t))]
                     [else s1])))])
    (for/fold ([m +inf.0])
              ([r (in-set s)]
               #:break (zero? m))
      (better m r))))

(define/contract (closest-cost baseCosts toppingCosts target)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer? exact-integer?)
  (define l
    (map (lambda (baseCost) (i-closest-res (- target baseCost) toppingCosts)) baseCosts))
  (- target (for/fold ([m +inf.0])
                      ([r (in-list l)])
              (better m r))))

(require racket/trace)

; by piscesciurus
(trace-define (closest-cost-1 baseCosts toppingCosts target)
  (argmin (lambda (x) (+ x (* 114514 (abs (- target x)))))
          (if (null? baseCosts)
              (if (null? toppingCosts)
                  '(0)
                  (for/list ((x (in-range 3)))
                    (+ (closest-cost-1 null (cdr toppingCosts) (- target (* x (car toppingCosts))))
                       (* x (car toppingCosts)))))
              (for/list ((x baseCosts))
                (+ (closest-cost-1 null toppingCosts (- target x)) x)))))

; (closest-cost '(1 7) '(3 4) 10)

(closest-cost-1 '(2 3) '(4 5 100) 18)