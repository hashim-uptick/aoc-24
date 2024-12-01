#lang racket

(define inp (with-input-from-file "input.txt" port->lines))

(define-values (left right)
  (for/lists (l r #:result (values (sort l <) (sort r <)))
             ([line inp])
    (apply values (map string->number (string-split line)))))

;; part 1

(define distances (map (compose abs -) left right))
(define total-distance (apply + distances))

total-distance

;; part 2

(define similarities (map (λ(x) (* x (count (λ(y) (= y x)) right))) left))
(define total-similarity (apply + similarities))

total-similarity
