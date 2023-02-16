#lang racket

; ; simple compare, TLE
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

; ; hash without binary search, TLE
; (define (s->hash s)
;   (define h (make-hash))
;   (for ([c (in-string s (sub1 (string-length s)) -1 -1)]
;         [i (in-range (sub1 (string-length s)) -1 -1)])
;     (hash-set! h c (cons i (hash-ref h c empty))))
;   h)

; (define (subeq? h word)
;   (define (iter pos wordres)
;     (cond [(boolean? pos) false]
;           [(null? wordres) true]
;           [else (iter (findf (lambda (x)
;                                (> x pos))
;                              (hash-ref h (car wordres) (list -1)))
;                       (cdr wordres))]))
;   (iter -1 word))

; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (define h (s->hash s))
;   (count (lambda (word) (subeq? h (string->list word))) words))

; ; multi ptr, TLE
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

; ; multi ptr with string-ref, TLE
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

; ; still not works
; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (count null?
;          (for/fold ([ws (map string->list words)])
;                    ([c s])
;            (map (lambda (w)
;                   (if (and (pair? w)
;                            (equal? c (car w)))
;                       (cdr w)
;                       w))
;                 ws))))

; ; multi ptr with hash, works
; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (define h (make-hash))
;   (for ([w (map string->list words)])
;     (hash-set! h (car w) (cons (cdr w) (hash-ref h (car w) empty))))
;   (for/sum ([c s])
;     (let ([q (hash-ref h c empty)])
;       (hash-remove! h c)
;       (count (lambda (x)
;                (unless (null? x)
;                  (hash-set! h (car x) (cons (cdr x) (hash-ref h (car x) empty))))
;                (null? x))
;              q))))

; ; pure function! final version
; (define/contract (num-matching-subseq s words)
;   (-> string? (listof string?) exact-integer?)
;   (hash-ref (for/fold ([h (for/fold ([h (hash)])
;                                     ([w (map string->list words)])
;                             (hash-set h (car w) (cons (cdr w) (hash-ref h (car w) empty))))])
;                       ([c s])
;               (for/fold ([hh (hash-remove h c)])
;                         ([ws (hash-ref h c empty)])
;                 (if (null? ws)
;                     (hash-update hh 0 add1 0)
;                     (hash-update hh (car ws)
;                                  (curry cons (cdr ws))
;                                  empty))))
;             0 0))

(define/contract (num-matching-subseq s words)
  (-> string? (listof string?) exact-integer?)
  (define h
    (foldl (λ (w h) (hash-update h (car w) (λ (l) (cons (cdr w) l)) null))
           (hasheq)
           (map string->list words)))
  (for/fold ([h h] [ss 0] #:result ss)
            ([c (in-string s)])
    (for/fold ([hh (hash-remove h c)] [ss ss])
              ([ws (in-list (hash-ref h c null))])
      (cond [(null? ws) (values hh (add1 ss))]
            [else
             (values (hash-update hh (car ws) (λ (l) (cons (cdr ws) l)) null)
                     ss)]))))

; ; from other people
; (define (num-matching-subseq s words)
;   (let ((p (make-vector 26 null)))
;     (for ((w words))
;       (let ([i (- (char->integer (string-ref w 0)) 97)])
;         (vector-set! p i (cons (cdr (string->list w)) (vector-ref p i)))))
;     (for/sum ((c s))
;       (let* ([i (- (char->integer c) 97)]
;              [q (vector-ref p i)])
;         (vector-set! p i null)
;         (for/sum ((w q))
;           (if (null? w)
;               1
;               (let ([j (- (char->integer (car w)) 97)])
;                 (vector-set! p j
;                              (cons (cdr w) (vector-ref p j)))
;                 0)))))))

(define s "dsahjpjauf")
(define words (list "ahjpjau" "ja" "ahbwzgqnuk" "tnmlanowax"))

(num-matching-subseq s words)