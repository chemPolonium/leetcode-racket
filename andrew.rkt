#lang racket

(define (point<? a b)
  (or (< (first a) (first b))
      (and (= (first a) (first b))
           (< (second a) (second b)))))

(define (point=? a b)
  (and (= (first a) (first b))
       (= (second a) (second b))))

(define (list<? l1 l2)
  (cond [(empty? l1) false]
        [(= (first l1) (first l2))
         (list<? (rest l1) (rest l2))]
        [else (< (first l1) (first l2))]))

(define (list=? l1 l2)
  (andmap = l1 l2))

(define (path-cross p1 p2 p3)
  (define x1 (- (first p2) (first p1)))
  (define y1 (- (second p2) (second p1)))
  (define x2 (- (first p3) (first p1)))
  (define y2 (- (second p3) (second p1)))
  (- (* x1 y2) (* y1 x2)))

(define (half-andrew points)
  (define (recr prev curr rest-points)
    (if (empty? rest-points)
        (values true (list curr))
        (let ([next (first rest-points)])
          (if (negative? (path-cross prev curr next))
              (values false rest-points)
              (let-values ([(succeed rest-or-valid)
                            (recr curr next (rest rest-points))])
                (if succeed
                    (values true (cons curr rest-or-valid))
                    (recr prev curr rest-or-valid)))))))
  (define sorted (sort points point<?))
  (recr (car sorted) (car sorted) (cdr sorted)))

(define (andrew points)
  (define sorted (sort points point<?))
  (define path-left (first sorted))
  (define (recr prev curr wasted remained)
    (if (empty? remained)
        (if (point=? curr path-left)
            (values wasted empty)
            (recr prev curr empty (append wasted (list path-left))))
        (let ([next (first remained)])
          (if (negative? (path-cross prev curr next))
              (values (cons curr wasted) remained)
              (let-values ([(next-wasted next-remained)
                            (recr curr next wasted (rest remained))])
                (if (point=? next (first next-wasted))
                    (recr prev curr next-wasted next-remained)
                    (values next-wasted (cons curr next-remained))))))))
  (recr path-left path-left empty (rest sorted)))

(andrew '((0 0) (3 -1) (1 4) (2 5) (4 1) (4 2) (6 4)))
