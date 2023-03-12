#lang racket

(define/contract (min-number-of-hours initialEnergy initialExperience energy experience)
  (-> exact-integer? exact-integer? (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (let iter ([my-energy initialEnergy]
             [my-experience initialExperience]
             [energy energy]
             [experience experience]
             [hours 0])
    (if (null? energy)
        hours
        (let* ([current-energy (first energy)]
               [current-experience (first experience)]
               [train-energy (max 0 (- (add1 current-energy) my-energy))]
               [train-experience (max 0 (- (add1 current-experience) my-experience))]
               [current-train (+ train-energy train-experience)]
               [my-energy (+ my-energy train-energy (- current-energy))]
               [my-experience (+ my-experience train-experience current-experience)])
          (iter my-energy my-experience (rest energy) (rest experience) (+ hours current-train))))))