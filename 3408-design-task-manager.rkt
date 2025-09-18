#lang racket

(require data/heap)

(define task-manager%
  (class object%
    (super-new)
    (define task-heap (make-heap (lambda (task1 task2)
                                   (define task-id1 (second task1))
                                   (define priority1 (third task1))
                                   (define task-id2 (second task2))
                                   (define priority2 (third task2))
                                   (cond [(> priority1 priority2) true]
                                         [(< priority1 priority2) false]
                                         [else (> task-id1 task-id2)]))))
    (define user-hash (make-hasheq))
    (define priority-hash (make-hasheq))
    ; tasks : (listof (listof exact-integer?))
    (init-field
     tasks)
    (for ([task (in-list tasks)])
      (match task
        [(list user-id task-id priority)
         (add user-id task-id priority)]))
    ; add : exact-integer? exact-integer? exact-integer? -> void?
    (define/public (add user-id task-id priority)
      (heap-add! task-heap (list user-id task-id priority))
      (hash-set! user-hash task-id user-id)
      (hash-set! priority-hash task-id priority))
    ; edit : exact-integer? exact-integer? -> void?
    (define/public (edit task-id new-priority)
      (hash-set! priority-hash task-id new-priority)
      (heap-add! task-heap (list (hash-ref user-hash task-id) task-id new-priority)))
    ; rmv : exact-integer? -> void?
    (define/public (rmv task-id)
      (hash-set! user-hash task-id -1)
      (hash-set! priority-hash task-id -1))
    ; exec-top : -> exact-integer?
    (define/public (exec-top)
      (let iter ()
        (if (zero? (heap-count task-heap))
            -1
            (match (heap-min task-heap)
              [(list user-id task-id priority)
               (cond [(and (= user-id (hash-ref user-hash task-id))
                           (= priority (hash-ref priority-hash task-id)))
                      (hash-set! user-hash task-id -1)
                      (hash-set! priority-hash task-id -1)
                      (heap-remove-min! task-heap)
                      user-id]
                     [else
                      (heap-remove-min! task-heap)
                      (iter)])]))))))

;; Your task-manager% object will be instantiated and called as such:
;; (define obj (new task-manager% [tasks tasks]))
;; (send obj add user-id task-id priority)
;; (send obj edit task-id new-priority)
;; (send obj rmv task-id)
;; (define param_4 (send obj exec-top))

(define obj (new task-manager% [tasks '((1 101 10) (2 102 20) (3 103 15))]))
(send obj add 4 104 5)
(send obj edit 102 8)
(send obj exec-top)
(send obj rmv 101)
(send obj add 5 105 15)
(send obj exec-top)
