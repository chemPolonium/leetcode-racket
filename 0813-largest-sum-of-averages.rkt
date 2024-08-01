#lang racket

; (define (acc-mean l)
;   (define (recr i s ls)
;     (cond [(null? ls) ls]
;           [else (let ([ns (+ s (car ls))])
;                   (cons (/ ns i)
;                         (recr (add1 i) ns (cdr ls))))]))
;   (recr 1 0.0 l))

; (define (acc-sum l)
;   (define (recr s ls)
;     (cond [(null? ls) ls]
;           [else (let ([ns (+ s (car ls))])
;                   (cons ns (recr ns (cdr ls))))]))
;   (recr 0 l))

(define (make-mean-hash nums)
  (define (h1 i l h)
    (define (iter1 i j s l h)
      (if (null? l)
          h
          (let ([ns (+ s (car l))])
            (iter1 i (add1 j) ns (cdr l) (hash-set h (cons i j) ns)))))
    (iter1 i (add1 i) 0.0 l h))
  (define (iter i l h)
    (if (null? l)
        h
        (iter (add1 i) (cdr l) (h1 (add1 i) (cdr l) h))))
  (for/hash ([(c v) (in-hash (iter -1 (cons 0 nums) (hash)))])
    (values c (match c [(cons i j) (/ v (- j i))]))))

; for higher Racket
; (hash-map/copy (iter -1 (cons 0 nums) (hash))
;                  (lambda (c v)
;                    (values c (match c [(cons i j) (/ v (- j i))]))))

(define/contract (largest-sum-of-averages nums k)
  (-> (listof exact-integer?) exact-integer? flonum?)
  (define l (length nums))
  (define h (make-mean-hash nums))
  (define v (for/vector ([j (in-range 1 (add1 l))])
              (hash-ref h (cons 0 j))))
  (for* ([j (in-range 2 (add1 k))]
         [i (in-range (sub1 l) -1 -1)]
         [x (in-range 0 i)])
    (vector-set! v i (max (vector-ref v i)
                          (+ (vector-ref v x) (hash-ref h (cons (add1 x) (add1 i)))))))
  (vector-ref v (sub1 l)))

(define nums '(1.0 2.0 3.0 4.0 5.0))

; (acc-mean nums)

; (acc-sum nums)

(define h (make-mean-hash nums))
(define v (for/vector ([j (in-range 1 6)])
            (hash-ref h (cons 0 j))))

(largest-sum-of-averages '(9 1 2 3 9) 3)

(largest-sum-of-averages '(1 2 3 4 5 6 7) 4)