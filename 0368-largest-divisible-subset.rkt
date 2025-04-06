#lang racket

(require data/order)
(require data/splay-tree)
(require math/number-theory)

(define/contract (largest-divisible-subset nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define d (make-splay-tree (order 'r exact-integer? = > <)))
  (define h (make-hasheq))
  (for ([num (in-list (sort nums <))])
    (let iter ([i (dict-iterate-first d)])
      (cond [(not i)
             (dict-set! d 1 (cons num (dict-ref d 1 empty)))]
            [(ormap (lambda (x) (and (divides? x num)
                                     (hash-set! h num x)
                                     true))
                    (dict-iterate-value d i))
             (define k (add1 (dict-iterate-key d i)))
             (define v (dict-ref d k empty))
             (dict-set! d k (cons num v))]
            [else
             (iter (dict-iterate-next d i))])))
  (define start
    (car (dict-iterate-value d (dict-iterate-first d))))
  (let iter ([i start] [res empty])
    (if (hash-has-key? h i)
        (iter (hash-ref h i) (cons i res))
        (cons i res))))

(largest-divisible-subset '(1 2 3))
(largest-divisible-subset '(1 2 4 8))
