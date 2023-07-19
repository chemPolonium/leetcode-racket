#lang racket

(define/contract (robot-sim commands obstacles)
  (-> (listof exact-integer?) (listof (listof exact-integer?)) exact-integer?)
  (define obstacles-set (list->set obstacles))
  (define orientation 'n)
  (define (rot-left)
    (set! orientation (match orientation ['n 'w] ['w 's] ['s 'e] ['e 'n])))
  (define (rot-right)
    (set! orientation (match orientation ['n 'e] ['e 's] ['s 'w] ['w 'n])))
  (define position-x 0)
  (define position-y 0)
  (define max-dis 0)
  (define (square x) (* x x))
  (define (refresh-max-dis)
    (set! max-dis (max max-dis (+ (square position-x) (square position-y)))))
  (for ([c (in-list commands)])
    (cond [(positive? c)
           (for ([_ (in-range c)])
             (define next-position-x
               (match orientation
                 ['w (sub1 position-x)]
                 ['e (add1 position-x)]
                 [_ position-x]))
             (define next-position-y
               (match orientation
                 ['n (add1 position-y)]
                 ['s (sub1 position-y)]
                 [_ position-y]))
             (when (not (set-member? obstacles-set
                                     (list next-position-x next-position-y)))
               (set! position-x next-position-x)
               (set! position-y next-position-y)
               (refresh-max-dis)))]
          [(= c -2) (rot-left)]
          [else (rot-right)]))
  max-dis)

(robot-sim '(4 -1 4 -2 4) '((2 4)))
