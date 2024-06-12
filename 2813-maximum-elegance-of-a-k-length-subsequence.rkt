#lang racket

(define/contract (find-maximum-elegance items k)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer?)
  (define (square x) (* x x))
  (define (item-profit item) (first item))
  (define (item-category item) (second item))
  (define h (make-hasheq))
  (define sorted-items (sort items > #:key item-profit))
  (define-values (first-k-items rest-items)
    (split-at sorted-items k))
  (define first-k-profits (apply + (map item-profit first-k-items)))
  (for ([item (in-list first-k-items)])
    (hash-update! h (item-category item) add1 0))
  (define first-k-categories (hash-count h))
  (define first-k-repetitive-items
    (reverse (filter (λ (item) (> (hash-ref h (item-category item)) 1)) first-k-items)))
  (for/fold ([repetitive-items first-k-repetitive-items]
             [profits first-k-profits]
             [categories first-k-categories]
             [max-elegance (+ first-k-profits (square first-k-categories))]
             #:result max-elegance)
            ([item (in-list rest-items)]
             #:unless (hash-has-key? h (item-category item))
             #:break (empty? repetitive-items))
    (let iter ([repetitive-items repetitive-items])
      (cond [(empty? repetitive-items)
             (values empty profits categories max-elegance)]
            [(= 1 (hash-ref h (item-category (first repetitive-items))))
             (iter (rest repetitive-items))]
            [else
             (define repetitive-item (first repetitive-items))
             (hash-update! h (item-category repetitive-item) sub1)
             (hash-set! h (item-category item) 1)
             (define n-profits (+ profits (item-profit item) (- (item-profit repetitive-item))))
             (define n-categories (add1 categories))
             (define n-elegance (+ n-profits (square n-categories)))
             (define n-max-elegance (max max-elegance n-elegance))
             (values (rest repetitive-items)
                     n-profits
                     n-categories
                     n-max-elegance)]))))

(find-maximum-elegance '((3 2) (5 1) (10 1)) 2)
(find-maximum-elegance '((3 1) (3 1) (2 2) (5 3)) 3)
(find-maximum-elegance '((1 1) (2 1) (3 1)) 3)
(find-maximum-elegance '((3 4) (8 4) (2 2) (1 3)) 2)
