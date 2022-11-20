#lang racket

(define/contract (unequal-triplets nums)
  (-> (listof exact-integer?) exact-integer?)
  (let* ([v (list->vector (hash-values (for/fold ([h (hash)])
                                                 ([n nums])
                                         (hash-update h n add1 0))))]
         [l (vector-length v)])
    (for*/sum ([i (range l)]
               [j (range (add1 i) l)]
               [k (range (add1 j) l)])
      (* (vector-ref v i) (vector-ref v j) (vector-ref v k)))))

(unequal-triplets '(4 4 2 4 3))