#lang racket

(define/contract (count-different-subsequence-gc-ds nums)
  (-> (listof exact-integer?) exact-integer?)
  (define numset (list->seteq nums))
  (define (has-num? x) (set-member? numset x))
  (define maxnum (foldl max 0 nums))
  (define ans 0)
  (for ([i (in-range 1 (add1 maxnum))])
    (define subgcd 0)
    (for ([j (in-range i (add1 maxnum) i)]
          #:when (has-num? j)
          #:break (= subgcd i))
      (cond [(zero? subgcd) (set! subgcd j)]
            [else (set! subgcd (gcd subgcd j))])
      (when (= subgcd i)
        (set! ans (add1 ans)))))
  ans)

; (count-different-subsequence-gc-ds '(4 6 16))
(count-different-subsequence-gc-ds '(6 10 3))