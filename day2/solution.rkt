#lang racket

(define inp
  (map (compose (curry map string->number) string-split)
       (with-input-from-file "input.txt" port->lines)))

(define (safe? l)
  (let ([decr? (< (second l) (first l))])
    (andmap (λ(x y) (and (equal? decr? (< y x))
                         (let ([diff (abs (- y x))])
                           (and (<= diff 3)
                                (>= diff 1)))))
            (drop-right l 1) (rest l))))

(count safe? inp)

(define (lenient-safe? l)
  (or (safe? l)
      (ormap safe?
             (map (λ(i) (append (take l i) (drop l (add1 i))))
                  (range (length l))))))

(count lenient-safe? inp)

(provide (all-defined-out))
