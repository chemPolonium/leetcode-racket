#lang racket

(require data/heap)

(define/contract (rearrange-barcodes barcodes)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define code-queue (make-heap (λ (x y) (> (cdr x) (cdr y)))))
  (heap-add-all! code-queue
                 (hash->list (foldl (λ (i h) (hash-update h i add1 0)) (hash) barcodes)))
  (let iter ([l (let ([c (heap-min code-queue)])
                  (heap-remove-min! code-queue)
                  (heap-add! code-queue (cons (car c) (sub1 (cdr c))))
                  (list (car c)))])
    (let ([c (heap-min code-queue)])
      (if (zero? (cdr c))
          l
          (begin
            (heap-remove-min! code-queue)
            (if (= (car l) (car c))
                (let ([c1 (heap-min code-queue)])
                  (heap-remove-min! code-queue)
                  (heap-add! code-queue c)
                  (heap-add! code-queue (cons (car c1) (sub1 (cdr c1))))
                  (iter (cons (car c1) l)))
                (begin (heap-add! code-queue (cons (car c) (sub1 (cdr c))))
                       (iter (cons (car c) l)))))))))