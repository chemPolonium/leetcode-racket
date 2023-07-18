#lang racket

(require data/heap)

;;; TIME OUT
; (require data/interval-map)
; (define/contract (min-interval intervals queries)
;   (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof exact-integer?))
;   (define r (make-interval-map))
;   (interval-map-set! r 0 114514 -1)
;   (for ([i (in-list (sort intervals > #:key (lambda (x) (- (cadr x) (car x)))))])
;     (interval-map-set! r (car i) (add1 (cadr i)) (- (cadr i) (car i) -1)))
;   (map (lambda (q) (interval-map-ref r q)) queries))

;;; TIME OUT
; (define/contract (min-interval intervals queries)
;   (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof exact-integer?))
;   (define (vector-lower-bound-key vec value key)
;     (let iter ([start 0]
;                [end (vector-length vec)])
;       (if (= start end)
;           start
;           (let* ([i (quotient (+ start end) 2)]
;                  [it (key (vector-ref vec i))])
;             (if (< it value)
;                 (iter (add1 i) end)
;                 (iter start i))))))
;   (define (vector-upper-bound-key vec value key)
;     (let iter ([start 0]
;                [end (vector-length vec)])
;       (if (= start end)
;           start
;           (let* ([i (quotient (+ start end) 2)]
;                  [it (key (vector-ref vec i))])
;             (if (> it value)
;                 (iter start i)
;                 (iter (add1 i) end))))))
;   (define d-intervals
;     (sort (let iter ([l null] [intervals intervals])
;             (cond [(null? intervals) l]
;                   [else
;                    (define interval (car intervals))
;                    (iter (cons (cons (- (cadr interval) (car interval) -1) interval) l)
;                          (cdr intervals))]))
;           > #:key car))
;   (define queries-i-vec
;     (list->vector
;      (sort (let iter ([l null] [i 0] [queries queries])
;              (cond ([null? queries] l)
;                    [else (iter (cons (cons (car queries) i) l)
;                                (add1 i)
;                                (cdr queries))]))
;            < #:key car)))
;   (define ans-vec (make-vector (vector-length queries-i-vec) -1))
;   (for ([d-interval (in-list d-intervals)])
;     (define interval-left (cadr d-interval))
;     (define interval-right (caddr d-interval))
;     (define l (vector-lower-bound-key queries-i-vec interval-left car))
;     (define r (vector-upper-bound-key queries-i-vec interval-right car))
;     (for ([i (in-range l r)])
;       (vector-set! ans-vec (cdr (vector-ref queries-i-vec i)) (car d-interval))))
;   (vector->list ans-vec))

(define/contract (min-interval intervals queries)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof exact-integer?))
  (define (interval-length interval)
    (add1 (- (cadr interval) (car interval))))
  (define interval-heap
    (make-heap (lambda (a b) (<= (interval-length a) (interval-length b)))))
  (define sorted-intervals
    (sort intervals
          (lambda (a b)
            (or (< (car a) (car b))
                (and (= (car a) (car b)) (< (cadr a) (cadr b)))))))
  (define-values (queries-i l)
    (let iter ([l null] [i 0] [queries queries])
      (cond ([null? queries] (values l i))
            [else (iter (cons (cons (car queries) i) l)
                        (add1 i)
                        (cdr queries))])))
  (define sorted-queries-i
    (sort queries-i < #:key car))
  (define ans-vec (make-vector l -1))
  (let iter1 ([queries sorted-queries-i] [intervals sorted-intervals])
    (cond [(or (and (null? intervals) (zero? (heap-count interval-heap)))
               (null? queries))
           (vector->list ans-vec)]
          [else
           (define query (car queries))
           (cond [(and (pair? intervals) (<= (caar intervals) (car query)))
                  (heap-add! interval-heap (car intervals))
                  (iter1 queries (cdr intervals))]
                 [else
                  (let iter2 ()
                    (cond [(zero? (heap-count interval-heap))
                           (iter1 (cdr queries) intervals)]
                          [else
                           (define current-interval (heap-min interval-heap))
                           (cond [(< (cadr current-interval) (car query))
                                  (heap-remove-min! interval-heap)
                                  (iter2)]
                                 [else
                                  (vector-set! ans-vec
                                               (cdr query)
                                               (interval-length current-interval))
                                  (iter1 (cdr queries) intervals)])]))])]))
  (vector->list ans-vec))

(min-interval '((1 4) (2 4) (3 6) (4 4)) '(2 3 4 5))
