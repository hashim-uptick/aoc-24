#lang racket

(define inp
  (map (compose (curry map string->number) string-split)
       (with-input-from-file "input.txt" port->lines)))

(define (n-safe? max-errors l)
  (define (inner decr?)
    (let loop ([errors 0]
               [consumed '()]
               [remaining l])
      (cond [(> errors max-errors) #f]
            [(null? consumed)
             (loop errors
                   (cons (first remaining) consumed)
                   (rest remaining))]
            [else (or (null? remaining)
                      (let* ([prev (first consumed)]
                             [next (first remaining)]
                             [diff (abs (- next prev))])
                        (if (and (<= diff 3)
                                 (>= diff 1)
                                 (equal? decr? (< next prev)))
                            (loop errors
                                  (cons next consumed)
                                  (rest remaining))
                            (or (loop (add1 errors)
                                      consumed
                                      (rest remaining))
                                (loop (add1 errors)
                                      (rest consumed)
                                      remaining)))))])))
  (or (inner #t) (inner #f)))

(count (curry n-safe? 0) inp)
(count (curry n-safe? 1) inp)

(provide n-safe?)
