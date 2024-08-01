#lang racket

(define (transpose xss)
  (apply map list xss))

(define (back-length n mines-this-row)
  (define (iter pos prev-len result)
    (if (= pos n)
        result
        (let ([this-len (if (member pos mines-this-row)
                            0
                            (add1 prev-len))])
          (iter (add1 pos) this-len (cons this-len result)))))
  (reverse (iter 0 0 '())))

(define (mines-in-row row-num mines)
  (map second (filter (lambda (mine)
                        (= row-num (first mine)))
                      mines)))

(define (back-length-grid n mines)
  (map (lambda (row)
         (back-length n (mines-in-row row mines)))
       (range n)))

(define (back-length-grid-reverse n mines)
  (map reverse
       (back-length-grid
        n
        (map (lambda (mine)
               (list (first mine)
                     (- n 1 (second mine))))
             mines))))

(define (back-length-grid-transpose n mines)
  (transpose (back-length-grid
              n
              (map (lambda (mine)
                     (list (second mine)
                           (first mine)))
                   mines))))

(define (back-length-grid-reverse-transpose n mines)
  (transpose (map reverse
                  (back-length-grid
                   n
                   (map (lambda (mine)
                          (list (second mine)
                                (- n 1 (first mine))))
                        mines)))))

(define (all-length-grid n mines)
  (append-map (lambda (a b c d)
                (map list a b c d))
              (back-length-grid n mines)
              (back-length-grid-reverse n mines)
              (back-length-grid-transpose n mines)
              (back-length-grid-reverse-transpose n mines)))

(define (min-length-grid n mines)
  (map (lambda (x) (foldl min n x))
       (all-length-grid n mines)))

(define/contract (order-of-largest-plus-sign n mines)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (foldl max 0 (min-length-grid n mines)))

(define mines '((0 0)))
(define n 1)

(back-length-grid
 n
 (map (lambda (mine)
        (list (second mine)
              (first mine)))
      mines))

(transpose (back-length-grid
            n
            (map (lambda (mine)
                   (list (second mine)
                         (first mine)))
                 mines)))

(back-length-grid n mines)
(back-length-grid-reverse n mines)
(back-length-grid-transpose n mines)
(back-length-grid-reverse-transpose n mines)

(map (lambda (a b c d) (map list a b c d))
     (back-length-grid n mines)
     (back-length-grid-reverse n mines)
     (back-length-grid-transpose n mines)
     (back-length-grid-reverse-transpose n mines))

(map (lambda (x) (foldl min n x))
     (all-length-grid n mines))

(min-length-grid n mines)

(order-of-largest-plus-sign n mines)
; (order-of-largest-plus-sign 1 '((0 0)))