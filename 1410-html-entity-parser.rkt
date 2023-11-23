#lang racket

(define/contract (entity-parser text)
  (-> string? string?)
  (let* ([text (string-replace text "&quot;" "\"")]
         [text (string-replace text "&apos;" "'")]
         [text (string-replace text "&gt;" ">")]
         [text (string-replace text "&lt;" "<")]
         [text (string-replace text "&frasl;" "/")]
         [text (string-replace text "&amp;" "&")])
    text))

(entity-parser "&amp; is an HTML entity but &ambassador; is not.")
