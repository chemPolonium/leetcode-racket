#lang racket

(require data/gvector)

; (define/contract (combination-sum candidates target)
;   (-> (listof exact-integer?) exact-integer? (listof (listof exact-integer?)))
;   (define dp (make-vector (add1 target) null))
;   (vector-set! dp 0 '(()))
;   (for* ([c (in-list candidates)]
;          [t (in-inclusive-range (sub1 target) 0 -1)]
;          [a (in-list (vector-ref dp t))]
;          [k (in-inclusive-range 1 (quotient (- target t) c))])
;     (define nt (+ (* k c) t))
;     (when (>= target nt)
;       (vector-set! dp nt (cons (append (make-list k c) a)
;                                (vector-ref dp nt)))))
;   (vector-ref dp target))

(define/contract (combination-sum candidates target)
  (-> (listof exact-integer?) exact-integer? (listof (listof exact-integer?)))
  (define ans (make-gvector))
  (let loop ([t target] [cs (sort candidates <)] [path null])
    (if (zero? t)
        (gvector-add! ans path)
        (let loop1 ([t t] [cs cs] [path path])
          (when (and (pair? cs) (>= t (car cs)))
            (loop (- t (car cs)) cs (cons (car cs) path))
            (loop1 t (cdr cs) path)))))
  (gvector->list ans))

(combination-sum '(2 3 6 7) 7)
