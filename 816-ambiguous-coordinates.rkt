#lang racket

(define (x-str str)
  (map (curry substring str 0) (range 1 (string-length str))))

(define (y-str str)
  (map (curry substring str) (range 1 (string-length str))))

(define (all-xy-pairs str)
  (if (= (string-length str) 1)
      str
      (map list (x-str str) (y-str str))))

(define (whole-str str)
  (map (curry substring str 0) (range 1 (add1 (string-length str)))))

(define (decimal-str str)
  (map (curry substring str) (range 1 (add1 (string-length str)))))

(define (all-wd-pairs str)
  (map list (whole-str str) (decimal-str str)))

(define (whole-valid? str)
  (cond [(= (string-length str) 1) true]
        [(eq? (first (string->list str)) #\0) false]
        [else true]))

(define (decimal-valid? str)
  (or (zero? (string-length str))
      (not (eq? (last (string->list str)) #\0))))

(define (wd-pair-valid? wd-pair)
  (let ([whole (car wd-pair)]
        [decimal (cadr wd-pair)])
    (and (whole-valid? whole)
         (decimal-valid? decimal))))

(define (wd-pair->str wd-pair)
  (let ([whole (car wd-pair)]
        [decimal (cadr wd-pair)])
    (if (zero? (string-length decimal))
        whole
        (string-append whole "." decimal))))

(define (valid-wd-pairs str)
  (filter wd-pair-valid?
          (all-wd-pairs str)))

(define (valid-nums str)
  (map wd-pair->str
       (valid-wd-pairs str)))

(define (valid-xy-pairs str)
  (map (curry map valid-nums) (all-xy-pairs str)))

(define (all-cons xy)
  (let ([x-list (car xy)]
        [y-list (cadr xy)])
    (append-map (lambda (x) (map (lambda (y) (string-append "(" x ", " y ")")) y-list)) x-list)))

(define/contract (ambiguous-coordinates s)
  (-> string? (listof string?))
  (append-map all-cons
              (valid-xy-pairs (substring s 1 (sub1 (string-length s))))))

; (define xy (caddr (valid-xy "123456")))
; (define x-list (car xy))
; (define y-list (cadr xy))

(all-wd-pairs "123")
(valid-wd-pairs "123")
(valid-nums "123")
(all-xy-pairs "123")
(valid-xy-pairs "123")

(define str "123")

(ambiguous-coordinates "(123)")