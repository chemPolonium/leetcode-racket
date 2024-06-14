#lang racket

(define/contract (longest-valid-substring word forbidden)
  (-> string? (listof string?) exact-integer?)
  (define f (list->set forbidden))
  (for/fold ([ans 0] [i 0] #:result ans)
            ([j (in-inclusive-range 1 (string-length word))])
    (define new-i
      (let iter ([new-i (sub1 j)])
        (cond [(set-member? f (substring word new-i j)) (add1 new-i)]
              [(or (= new-i i) (< new-i (- j 10))) i]
              [else (iter (sub1 new-i))])))
    (values (max ans (- j new-i)) new-i)))

(longest-valid-substring "cbaaaabc" '("aaa" "cb"))

(longest-valid-substring "leetcode" '("de" "le" "e"))
