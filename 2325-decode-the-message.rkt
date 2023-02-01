#lang racket

(define (char-next c)
  (integer->char (add1 (char->integer c))))

(define/contract (decode-message key message)
  (-> string? string? string?)
  (define h
    (let iter ([h (hash)] [l (string->list key)] [c #\a])
      (match l
        [(list-rest x xs) (cond [(or (not (char-alphabetic? x)) (hash-has-key? h x)) (iter h xs c)]
                                [else (iter (hash-set h x c) xs (char-next c))])]
        [_ h])))
  (list->string (for/list ([c (in-string message)])
                  (if (char-alphabetic? c) (hash-ref h c) c))))

(decode-message "the quick brown fox jumps over the lazy dog" "vkbs bs t suepuv")