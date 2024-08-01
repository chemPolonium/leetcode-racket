#lang racket

(define/contract (find-repeated-dna-sequences s)
  (-> string? (listof string?))
  (define (nb->int nb)
    (match nb
      [#\A 0]
      [#\C 1]
      [#\G 2]
      [#\T 3]))
  (define (dna-init)
    (for/fold ([acc 0])
              ([c (in-string s 0 9)])
      (+ (arithmetic-shift acc 2)
         (nb->int c))))
  (define (repeated-dna)
    (for/fold ([p-dna (dna-init)]
               [dna-set (seteq)]
               [ans (set)]
               #:result (set->list ans))
              ([(c i) (in-indexed (in-string s 9))])
      (define c-dna
        (+ (bitwise-and 1048575
                        (arithmetic-shift p-dna 2))
           (nb->int c)))
      (if (set-member? dna-set c-dna)
          (values c-dna dna-set (set-add ans (substring s i (+ i 10))))
          (values c-dna (set-add dna-set c-dna) ans))))
  (if (<= (string-length s) 10)
      empty
      (repeated-dna)))

(find-repeated-dna-sequences "")
(find-repeated-dna-sequences "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT")
