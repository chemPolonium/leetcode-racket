#lang racket

(define/contract (max-score grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  ; 问题简化为求每一个数左上方最小值
  (define n (length (first grid)))
  ; 简化前使用二维数组记录每个位置含此位置及其左上最小值
  ; 上方：行小于
  ; 左方：列小于
  ; 简化后逐行求解，可以把内存从 m * n 变成 n
  ; 因为是记录最小值所以初始值设个大点的
  (define v (make-vector n 1919810))
  (for*/fold ([a -1919810])
             ([row (in-list grid)]
              [(c i) (in-indexed (in-list row))])
    ; 这个位置上方的最小值，因为还没更新所以 v 里面是上方的
    (define umin (vector-ref v i))
    ; 这个位置左方最小值，因为已经更新了所以 v 里面是左方的
    ; 最左侧直接退化成上方的，不用 if 可以省跳转
    (define lmin (vector-ref v (max 0 (sub1 i))))
    ; 更新此处的最小值
    (vector-set! v i (min c lmin umin))
    (max a (- c (min umin lmin)))))

(max-score '((9 5 7 3) (8 9 6 1) (6 7 14 3) (2 5 3 1)))
(max-score '((4 3 2) (3 2 1)))
