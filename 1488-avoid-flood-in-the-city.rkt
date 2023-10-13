#lang racket

(require data/heap)
(require data/queue)

(define/contract (avoid-flood rains)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define a (make-vector (length rains) -1))
  ; each lake a queue, most urgent day first out
  (define lake-days (make-hasheq))
  (for ([(r i) (in-indexed (in-list rains))])
    (enqueue! (hash-ref! lake-days r (make-queue)) i))
  (for ([q (in-hash-values lake-days)])
    (dequeue! q))
  ; heap: filled lake and most urgent day
  (define lake-heap (make-heap (λ (a b) (<= (cdr a) (cdr b)))))
  (for ([(r i) (in-indexed (in-list rains))]
        ; lake full, flood in the city
        #:break (not lake-heap))
    (cond [(and (zero? r) (zero? (heap-count lake-heap)))
           ; no rain, no urgent lake, just 1
           (vector-set! a i 1)]
          [(zero? r)
           ; no rain, pump the urgent lake
           (define urgent-lake (car (heap-min lake-heap)))
           (vector-set! a i urgent-lake)
           ; remove lake from heap
           (heap-remove-min! lake-heap)]
          [(and (positive? (heap-count lake-heap))
                (= i (cdr (heap-min lake-heap))))
           ; rain, and the urgent day comes
           ; (= r (car (heap-min lake-heap))) also ok
           (set! lake-heap #f)]
          [else
           (define next-day-queue (hash-ref lake-days r))
           ; rain, but not urgent lake
           (unless (queue-empty? next-day-queue)
             ; add this lake and most urgent day to heap
             (heap-add! lake-heap (cons r (dequeue! next-day-queue))))]))
  (if lake-heap (vector->list a) null))

(avoid-flood '(1 2 3 4))
(avoid-flood '(1 2 0 0 2 1))
(avoid-flood '(1 2 0 1 2))
(avoid-flood '(0 1 1))
(avoid-flood '(3 0 1 0 2 0 1 2 3))
