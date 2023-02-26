#lang racket

(define/contract (max-score-words words letters score)
  (-> (listof string?) (listof char?) (listof exact-integer?) exact-integer?)
  (define word-count
    (for/hash ([w (in-list words)])
      (values w (sequence-fold (λ (h c) (hash-update h c add1 0)) (hasheq) (in-string w)))))
  (define letters-hash
    (foldl (λ (c h) (hash-update h c add1 0)) (hasheq) letters))
  (define score-hash
    (foldl (λ (c s h)
             (hash-set h c s))
           (hasheq)
           (build-list 26 (λ (i) (integer->char (+ i 97)) ))
           score))
  (define (word-score w)
    (for/sum ([(c n) (in-hash (hash-ref word-count w))])
      (* n (hash-ref score-hash c))))
  (define (consume-word letters-hash word)
    (for/fold ([l-hash letters-hash])
              ([[c n] (in-hash (hash-ref word-count word))])
      (and l-hash
           (let ([r (- (hash-ref l-hash c 0) n)])
             (and (nonnegative-integer? r) (hash-set l-hash c r))))))
  (let iter ([ws words] [ls letters-hash])
    (cond [(null? ws) 0]
          [else
           (define w (car ws))
           (define new-ls (consume-word ls w))
           (max (if new-ls
                    (+ (word-score w) (iter (cdr ws) new-ls))
                    0)
                (iter (cdr ws) ls))])))

(define words '["dog" "cat" "dad" "good"])
(define letters '[#\a #\a #\c #\d #\d #\d #\g #\o #\o])
(define score '[1 0 9 5 0 0 3 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0])

(max-score-words words letters score)