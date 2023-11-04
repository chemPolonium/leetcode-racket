#lang racket

(define/contract (find-maximum-xor nums)
  (-> (listof exact-integer?) exact-integer?)
  (define max-num (apply max nums))
  (define highest-bit (arithmetic-shift 1 (integer-length max-num)))
  (define (bit-max-xor h l bit)
    (define n-bit (arithmetic-shift bit -1))
    (define-values (hh hl)
      (partition (λ (x) (positive? (bitwise-and n-bit x))) h))
    (define-values (lh ll)
      (partition (λ (x) (positive? (bitwise-and n-bit x))) l))
    (cond [(null? h)
           (bit-max-xor lh ll n-bit)]
          [(null? l)
           (bit-max-xor hh hl n-bit)]
          [(zero? n-bit)
           (bitwise-xor (car h) (car l))]
          [(and (null? (cdr h)) (null? (cdr l)))
           (bitwise-xor (car h) (car l))]
          [(and (null? hh) (null? ll))
           (bit-max-xor hl lh n-bit)]
          [(and (null? hl) (null? lh))
           (bit-max-xor hh ll n-bit)]
          [(and (null? hh) (null? lh))
           (bit-max-xor hl ll n-bit)]
          [(and (null? hl) (null? ll))
           (bit-max-xor hh lh n-bit)]
          [(null? hl)
           (max (bit-max-xor hh lh n-bit) (bit-max-xor hh ll n-bit))]
          [(null? hh)
           (max (bit-max-xor hl lh n-bit) (bit-max-xor hl ll n-bit))]
          [(null? lh)
           (max (bit-max-xor hh ll n-bit) (bit-max-xor hl ll n-bit))]
          [(null? ll)
           (max (bit-max-xor hh lh n-bit) (bit-max-xor hl lh n-bit))]
          [else
           (max (bit-max-xor hh ll n-bit) (bit-max-xor hl lh n-bit))]))
  (if (or (null? (cdr nums)) (apply = nums))
      0
      (bit-max-xor nums null highest-bit)))

(find-maximum-xor '(3 10 5 25 2 8))
(find-maximum-xor '(14 70 53 83 49 91 36 80 92 51 66 70))
(find-maximum-xor '(1))
