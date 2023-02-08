#lang racket

(define/contract (remove-subfolders folder)
  (-> (listof string?) (listof string?))
  (define root (make-hash))
  (define (add-folder! f)
    (let iter ([r root] [l (string-split f "/")])
      (cond [(not r)]
            [(null? (cdr l)) (hash-set! r (car l) #f)]
            [else (iter (hash-ref! r (car l) (make-hash)) (cdr l))])))
  (map add-folder! folder)
  (define (dtree->list r)
    (if (not r)
        '("")
        (for*/list ([(k v) (in-hash r)]
                    [s (dtree->list v)])
          (string-append "/" k s))))
  (dtree->list root))

; (remove-subfolders '["/a/b/c" "/a/b/ca" "/a/b/d"])
(remove-subfolders '["/a" "/a/b" "/c/d" "/c/d/e" "/c/f"])