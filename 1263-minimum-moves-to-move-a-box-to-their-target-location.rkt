#lang racket

; 什么牛马题目，摆了摆了，这个是计算最小人走步数的（最后输出的是箱子的步数）通不过

(define/contract (min-push-box grid)
  (-> (listof (listof char?)) exact-integer?)
  (define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
  (define grid-vec (list->vector (map list->vector grid)))
  (define grid-m (vector-length grid-vec))
  (define grid-n (vector-length (vector-ref grid-vec 0)))
  (define (grid-ref m n) (vec2d-ref grid-vec m n))
  (define start-r 0)
  (define start-c 0)
  (define target-r 0)
  (define target-c 0)
  (define box-init-r 0)
  (define box-init-c 0)
  (define current-state-hash (make-hash))
  (define next-state-hash (make-hash))
  (define achieved-state-hash (make-hash))
  (for* ([i (in-range grid-m)]
         [j (in-range grid-n)])
    (define c (grid-ref i j))
    (match c
      [#\T
       (set! target-r i)
       (set! target-c j)]
      [#\B
       (set! box-init-r i)
       (set! box-init-c j)]
      [#\S
       (set! start-r i)
       (set! start-c j)]
      [_ 'else]))
  (hash-set! current-state-hash (list start-r start-c box-init-r box-init-c) 0)
  (hash-set! achieved-state-hash (list start-r start-c box-init-r box-init-c) 0)
  (define (log-state! state steps)
    (unless (hash-has-key? achieved-state-hash state)
      (hash-set! next-state-hash state steps)
      (hash-set! achieved-state-hash state steps)))
  (define (invalid-pos? r c)
    (or (= r grid-m)
        (= r -1)
        (= c grid-n)
        (= c -1)
        (char=? #\# (grid-ref r c))))
  (let/cc return
    (define (move state steps direction)
      (match state
        [(list current-r current-c current-box-r current-box-c)
         (match direction
           [(list direction-r direction-c)
            (define new-r (+ current-r direction-r))
            (define new-c (+ current-c direction-c))
            (cond [(invalid-pos? new-r new-c)]
                  [(and (= new-r current-box-r) (= new-c current-box-c))
                   (define new-box-r (+ current-box-r direction-r))
                   (define new-box-c (+ current-box-c direction-c))
                   (unless (invalid-pos? new-box-r new-box-c)
                     (log-state! (list new-r new-c new-box-r new-box-c)
                                 (add1 steps))
                     (when (and (= new-box-r target-r) (= new-box-c target-c))
                       (return (add1 steps))))]
                  [else
                   (log-state! (list new-r new-c current-box-r current-box-c) steps)])])]))
    (do () ((hash-empty? current-state-hash) -1)
      (for ([(state steps) (in-hash current-state-hash)])
        (move state steps '(0 1))
        (move state steps '(1 0))
        (move state steps '(0 -1))
        (move state steps '(-1 0)))
      (set! current-state-hash next-state-hash)
      (set! next-state-hash (make-hash)))))
