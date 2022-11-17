#lang racket

; 朴素思想，超时
; (define (subeq? s word)
;   (define (iter sres wordres)
;     (cond [(null? wordres) true]
;           [(null? sres) false]
;           [(equal? (car sres) (car wordres))
;            (iter (cdr sres) (cdr wordres))]
;           [else
;            (iter (cdr sres) wordres)]))
;   (iter (string->list s) (string->list word)))

; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (count (lambda (word) (subeq? h word)) words))

; 哈希表不用二分查找，超时
(define (s->hash s)
  (define h (make-hash))
  (for ([c (in-string s (sub1 (string-length s)) -1 -1)]
        [i (in-range (sub1 (string-length s)) -1 -1)])
    (hash-set! h c (cons i (hash-ref h c empty))))
  h)

(define (subeq? h word)
  (define (iter pos wordres)
    (cond [(boolean? pos) false]
          [(null? wordres) true]
          [else (iter (findf (lambda (x)
                               (> x pos))
                             (hash-ref h (car wordres) (list -1)))
                      (cdr wordres))]))
  (iter -1 word))

(define/contract (num-matching-subseq s words)
  (-> string? (listof string?) exact-integer?)
  (define h (s->hash s))
  (count (lambda (word) (subeq? h (string->list word))) words))

; 多指针，还是超时
; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (count (negate non-empty-string?)
;          (for/fold ([ws words])
;                    ([c s])
;            (for/list ([w ws])
;              (if (and (non-empty-string? w)
;                       (equal? c (string-ref w 0)))
;                  (substring w 1)
;                  w)))))

; 基于字符位置的多指针，还是超时
; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (define wl
;     (for/fold ([l (make-list (length words) 0)])
;               ([c s])
;       (for/list ([w words]
;                  [i l])
;         (if (and (< i (string-length w))
;                  (equal? c (string-ref w i)))
;             (add1 i)
;             i))))
;   (for/sum ([i wl]
;             [w words])
;     (if (= i (string-length w))
;         1
;         0)))

(num-matching-subseq "abcde" (list "a" "bb" "acd" "ace"))