#lang racket

(define/contract (top-students positive_feedback negative_feedback report student_id k)
  (-> (listof string?) (listof string?) (listof string?) (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define positive-set (list->set positive_feedback))
  (define negative-set (list->set negative_feedback))
  (define (score r)
    (for/sum ([w (in-list (string-split r))])
      (cond [(set-member? positive-set w) 3]
            [(set-member? negative-set w) -1]
            [else 0])))
  (define score-list
    (for/list ([r (in-list report)]
               [i (in-list student_id)])
      (cons (score r) i)))
  (define sorted-score-list
    (sort score-list (λ (a b) (if (= (car a) (car b))
                                  (< (cdr a) (cdr b))
                                  (> (car a) (car b))))))
  (map cdr (take sorted-score-list k)))
