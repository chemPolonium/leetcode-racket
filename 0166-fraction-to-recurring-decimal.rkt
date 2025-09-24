#lang racket

(require data/gvector)

(define/contract (fraction-to-decimal numerator denominator)
  (-> exact-integer? exact-integer? string?)
  (define abs-numerator (abs numerator))
  (define abs-denominator (abs denominator))
  (define s (and (not (zero? numerator))
                 (xor (negative? numerator) (negative? denominator))))
  (define g (gcd abs-numerator abs-denominator))
  (define num (/ abs-numerator g))
  (define den (/ abs-denominator g))
  (define-values (int-part dec-num) (quotient/remainder num den))
  (define dec-vec (gvector))
  (define num-hash (make-hasheq))
  (define loop-begin
    (let iter ([num dec-num] [i 0])
      (cond [(zero? num) i]
            [(hash-ref num-hash num #f)]
            [else
             (define-values (q r) (quotient/remainder (* 10 num) den))
             (gvector-add! dec-vec q)
             (hash-set! num-hash num i)
             (iter r (add1 i))])))
  (define (extract-str start end)
    (build-string (- end start)
                  (lambda (i)
                    (integer->char (+ (gvector-ref dec-vec (+ start i)) 48)))))
  (define str-int (number->string int-part))
  (define str-prefix (extract-str 0 loop-begin))
  (define str-loop (extract-str loop-begin (gvector-count dec-vec)))
  (define str-dec
    (string-append str-prefix
                   (if (non-empty-string? str-loop)
                       (string-append "(" str-loop ")")
                       "")))
  (string-append (if s "-" "")
                 str-int
                 (if (zero? dec-num)
                     ""
                     (string-append "." str-dec))))

(fraction-to-decimal 1 2)
"0.5"
(fraction-to-decimal 2 1)
"2"
(fraction-to-decimal 4 333)
"0.(012)"
(fraction-to-decimal 1 6)
"0.1(6)"
(fraction-to-decimal -50 8)
"-6.25"
(fraction-to-decimal 0 -5)
"0"
