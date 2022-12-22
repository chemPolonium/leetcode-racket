#lang racket

;;; OUT OF TIME
;;; NOT A VALID ANSWER

(define/contract (max-score nums)
  (-> (listof exact-integer?) exact-integer?)

  (define gcd-map (make-hash))

  (define (mem-ind-gcd a b)
    (let ([key (if (< a b) (cons a b) (cons b a))])
      (unless (hash-has-key? gcd-map key)
        (hash-set! gcd-map key (gcd (vector-ref numv a) (vector-ref numv b))))
      (hash-ref gcd-map key)))

  (define score-map (make-hash))

  (define (mem-n-max-score i numset)
    (unless (hash-has-key? score-map (cons i numset))
      (hash-set! score-map (cons i numset) (n-max-score i numset)))
    (hash-ref score-map (cons i numset)))

  (define numv (list->vector nums))

  (define (n-max-score i numset)
    (if (= (set-count numset) 2)
        (* i (apply mem-ind-gcd (set->list numset)))
        (for*/fold ([m 0])
                   ([a (in-set numset)]
                    [b (in-set (set-remove numset a))])
          (max m
               (+ (* i (mem-ind-gcd a b))
                  (mem-n-max-score (add1 i)
                                   (set-remove (set-remove numset a) b)))))))

  (n-max-score 1 (list->set (range (length nums)))))

(max-score '(1373 1811 509 2503 113 421 79 1373 1811 509 2503 113 421 79))