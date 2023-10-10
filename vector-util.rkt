#lang racket

(define (vector-update! vec pos updater)
  (vector-set! vec pos (updater (vector-ref vec pos))))

(define (vector-binary-search vec value)
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        #f
        (let* ([i (quotient (+ start end) 2)]
               [it (vector-ref vec i)])
          (cond [(> it value) (iter start i)]
                [(< it value) (iter (add1 i) end)]
                [else i])))))

(define (vector-binary-search-key vec value key)
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        #f
        (let* ([i (quotient (+ start end) 2)]
               [it (key (vector-ref vec i))])
          (cond [(> it value) (iter start i)]
                [(< it value) (iter (add1 i) end)]
                [else i])))))

(define (vector-binary-search-cmp vec value cmp)
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        #f
        (let* ([i (quotient (+ start end) 2)]
               [comparison (cmp (vector-ref vec i) value)])
          (cond [(positive? comparison) (iter start i)]
                [(negative? comparison) (iter (add1 i) end)]
                [else i])))))

(define (vector-lower-bound vec
                            value
                            [start 0]
                            [end (vector-length vec)]
                            #:key [key identity]
                            #:< [< <])
  (let iter ([start start]
             [end end])
    (if (= start end)
        start
        (let* ([i (quotient (+ start end) 2)]
               [it (key (vector-ref vec i))]
               [comparison (< it value)])
          (if comparison
              (iter (add1 i) end)
              (iter start i))))))

; (define (vector-lower-bound vec value)
;   (let iter ([start 0]
;              [end (vector-length vec)])
;     (if (= start end)
;         start
;         (let* ([i (quotient (+ start end) 2)]
;                [it (vector-ref vec i)])
;           (if (< it value)
;               (iter (add1 i) end)
;               (iter start i))))))

(define (vector-lower-bound-key vec value key)
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        start
        (let* ([i (quotient (+ start end) 2)]
               [it (key (vector-ref vec i))])
          (if (< it value)
              (iter (add1 i) end)
              (iter start i))))))

(define (vector-lower-bound-cmp vec value cmp)
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        start
        (let* ([i (quotient (+ start end) 2)]
               [comparison (cmp (vector-ref vec i) value)])
          (if (negative? comparison)
              (iter (add1 i) end)
              (iter start i))))))

(define (vector-upper-bound vec
                            value
                            [start 0]
                            [end (vector-length vec)]
                            #:key [key identity]
                            #:< [< <])
  (let iter ([start start]
             [end end])
    (if (= start end)
        start
        (let* ([i (quotient (+ start end) 2)]
               [it (key (vector-ref vec i))]
               [comparison (< value it)])
          (if comparison
              (iter start i)
              (iter (add1 i) end))))))

;;; (VECTOR-BINARY-SEARCH <vector> <value> <cmp>)
;;;       -> exact, nonnegative integer or #F
;;;     (CMP <value1> <value2>) -> integer
;;;       positive -> VALUE1 > VALUE2
;;;       zero     -> VALUE1 = VALUE2
;;;       negative -> VALUE1 < VALUE2
;;;   Perform a binary search through VECTOR for VALUE, comparing each
;;;   element to VALUE with CMP.

; (define (vector-upper-bound vec value [cmp -])
;   (let iter ([current-ind 0]
;              [hl-dis (vector-length vec)])
;     (if (positive? hl-dis)
;         (let* ([step (quotient hl-dis 2)]
;                [next-ind (+ current-ind step)]
;                [comparison (cmp (vector-ref vec next-ind) value)])
;           (if (positive? comparison)
;               (iter current-ind step)
;               (iter (add1 next-ind) (- hl-dis step 1))))
;         current-ind)))

; (define (vector-lower-bound-s vec value [cmp -])
;   (let iter ([current-ind 0]
;              [hl-dis (vector-length vec)])
;     (if (positive? hl-dis)
;         (let* ([step (quotient hl-dis 2)]
;                [next-ind (+ current-ind step)]
;                [comparison (cmp (vector-ref vec next-ind) value)])
;           (if (negative? comparison)
;               (iter (add1 next-ind) (- hl-dis step 1))
;               (iter current-ind step)))
;         current-ind)))

(define (test-vector-search f)
  (define v #(1 3 5 7 9))
  (for ([i (in-list '(0 3 4 10))])
    (displayln (f v i))))

(define (time-vector-search f)
  (define l 10000000)
  (define v (build-vector l identity))
  (time (for ([i (in-range l)])
          (f v i))))

(define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
(define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
(define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))
(define (vec2d-update! vec m n updater) (vec2d-set! vec m n (updater (vec2d-ref vec m n))))

(struct ndvec (size data) #:transparent)
(define (make-ndvec size [v 0]) (ndvec size (make-vector (apply * size) v)))
(define (sub->ind size sub)
  (let iter ([size (cdr size)] [sub (cdr sub)] [acc (car sub)])
    (if (null? size) acc (iter (cdr size) (cdr sub) (+ (* acc (car size)) (car sub))))))
(define (ndvec-ref vec pos)
  (vector-ref (ndvec-data vec) (sub->ind (ndvec-size vec) pos)))
(define (ndvec-set! vec pos v)
  (vector-set! (ndvec-data vec) (sub->ind (ndvec-size vec) pos) v))
(define (ndvec-update! vec pos updater)
  (let* ([ind (sub->ind (ndvec-size vec) pos)]
         [v (vector-ref (ndvec-data vec) ind)])
    (vector-set! (ndvec-data vec) ind (updater v))))

(define (test-vector-binary-search)
  (test-vector-search vector-binary-search))

(define (test-vector-upper-bound)
  (test-vector-search vector-upper-bound))

(define (test-vector-lower-bound)
  (test-vector-search vector-lower-bound))

(define (time-vector-binary-search)
  (time-vector-search vector-binary-search))

(define (time-vector-binary-search-key)
  (time-vector-search (λ (v i) (vector-binary-search-key v i identity))))

(define (time-vector-binary-search-cmp)
  (time-vector-search (λ (v i) (vector-binary-search-cmp v i -))))
