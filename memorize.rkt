#lang racket

(define (memorize1 f)
  (let ([h (make-hash)])
    (lambda (arg)
      (if (hash-has-key? h arg)
          (hash-ref h arg)
          (let ([v (apply f arg)])
            (hash-set! h arg v)
            v)))))

(define (memorize f)
  (let ([h (make-hash)])
    (lambda args
      (if (hash-has-key? h args)
          (hash-ref h args)
          (let ([v (apply f args)])
            (hash-set! h args v)
            v)))))
