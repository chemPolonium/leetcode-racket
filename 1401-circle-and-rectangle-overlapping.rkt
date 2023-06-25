#lang racket

(define (check-overlap radius xCenter yCenter x1 y1 x2 y2)
  (>= (* radius radius)
      (apply + (map (λ (x) (* x x))
                    (filter positive? (list (- xCenter x2)
                                            (- x1 xCenter)
                                            (- yCenter y2)
                                            (- y1 yCenter)))))))