#lang racket

(define/contract (min-moves-to-seat seats students)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (apply + (map (lambda (a b) (abs (- a b))) (sort seats <) (sort students <))))

(min-moves-to-seat '(3 1 5) '(2 7 4))