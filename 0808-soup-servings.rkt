#lang racket

(define (iter h)
  (for/fold ([nh (make-hash)])
            ([(res pos) h])
    (cond [(and (positive? (car res))
                (positive? (cdr res)))
           (let ([updater (lambda (x) (+ x (/ pos 4)))])
             (begin (hash-update! nh
                                  (cons (- (car res) 100)
                                        (cdr res))
                                  updater 0)
                    (hash-update! nh
                                  (cons (- (car res) 75)
                                        (- (cdr res) 25))
                                  updater 0)
                    (hash-update! nh
                                  (cons (- (car res) 50)
                                        (- (cdr res) 50))
                                  updater 0)
                    (hash-update! nh
                                  (cons (- (car res) 25)
                                        (- (cdr res) 75))
                                  updater 0))
             nh)]
          [(and (positive? (car res))
                (<= (cdr res) 0))
           nh]
          [else
           (hash-set! nh res pos)
           nh])))

(define/contract (soup-servings n)
  (-> exact-integer? flonum?)
  (if (> n 4801)
      1.0
      (let ([fh (for/fold ([h (hash (cons n n) 1.0)])
                          ([i (in-range (add1 (/ n 50)))])
                  (iter h))])
        (for/sum ([(res pos) fh])
          (if (positive? (cdr res))
              pos
              (/ pos 2))))))

(define n 1)

(iter (hash (cons n n) 1.0))
(iter (iter (hash (cons n n) 1.0)))
(iter (iter (iter (hash (cons n n) 1.0))))

(soup-servings n)
