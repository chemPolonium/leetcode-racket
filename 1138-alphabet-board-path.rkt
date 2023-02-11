#lang racket

(define/contract (alphabet-board-path target)
  (-> string? string?)
  (define (char-pos c)
    (quotient/remainder (- (char->integer c) 97) 5))
  (define current-row 0)
  (define current-col 0)
  (apply string-append
         (for/list ([c (in-string target)])
           (define-values (row col) (char-pos c))
           (define row-move (if (> row current-row)
                                (make-string (- row current-row) #\D)
                                (make-string (- current-row row) #\U)))
           (define col-move (if (> col current-col)
                                (make-string (- col current-col) #\R)
                                (make-string (- current-col col) #\L)))
           (set! current-row row)
           (set! current-col col)
           (if (char=? c #\z)
               (string-append col-move row-move "!")
               (string-append row-move col-move "!")))))

(alphabet-board-path "leet")