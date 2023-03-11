#lang racket

(define/contract (find-longest-subarray array)
  (-> (listof string?) (listof string?))
  (define l
    (let iter ([c 0] [l array])
      (if (pair? l)
          (let ([c ((if (char-alphabetic? (string-ref (car l) 0)) add1 sub1) c)])
            (cons c (iter c (cdr l))))
          null)))
  (define-values (start end)
    (for/fold ([h (hasheq 0 -1)] [start 0] [end 0] [max-dis 0] #:result (values start end))
              ([(c i) (in-indexed (in-list l))])
      (if (hash-has-key? h c)
          (let* ([new-start (hash-ref h c)]
                 [new-dis (- i new-start)])
            (if (< max-dis new-dis)
                (values h new-start i new-dis)
                (values h start end max-dis)))
          (values (hash-set h c i) start end max-dis))))
  (take (drop array (add1 start)) (- end start)))

(find-longest-subarray '("A" "1"))
(find-longest-subarray '("A" "A"))