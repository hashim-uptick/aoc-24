#lang racket

(define inp (with-input-from-file "input.txt" port->lines))

(define-values (rules^ updates^) (splitf-at inp non-empty-string?))
(define updates (map (λ(s) (map string->number (string-split s ","))) (rest updates^)))

(define rules
  (for/fold ([rules (hash)])
            ([line rules^])
    (match-define (list before after) (map string->number (string-split line "|")))
    (hash-update rules before (λ(x) (cons after x)) '())))

(define (middle-value lst)
  (list-ref lst (floor (/ (length lst) 2))))

(define (valid? update)
  (for/and ([left update] [right (rest update)])
    (member right (hash-ref rules left))))

(for/sum ([update updates]
          #:when (valid? update))
  (middle-value update))

(for/sum ([update updates]
          #:when (not (valid? update)))
  (middle-value (sort update (λ(l r) (member r (hash-ref rules l))))))
