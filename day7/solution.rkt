#lang racket

(define inp (with-input-from-file "input.txt" port->lines))
(struct Eqn (ans terms))

(define eqns
  (map (λ(line)
         (define splits (string-split line))
         (define terms (map string->number (rest splits)))
         (define ans (string->number (string-trim (first splits) ":")))
         (Eqn ans terms))
       inp))

(define (valid? eqn ops)
  (define target (Eqn-ans eqn))
  (let loop ([remaining (rest (Eqn-terms eqn))]
             [ans (first (Eqn-terms eqn))])
    (if (null? remaining)
        (equal? target ans)
        (let ([next (first remaining)])
          (ormap (λ(op) (loop (rest remaining) (op ans next))) ops)))))

;; part 1 ; 975671981569
(for/sum ([eqn eqns]
          #:when (valid? eqn (list + *)))
  (Eqn-ans eqn))

;; part 2 ; 223472064194845
(define (concat-digits a b)
  (string->number (string-append (number->string a) (number->string b))))

(for/sum ([eqn eqns]
          #:when (valid? eqn (list + * concat-digits)))
  (Eqn-ans eqn))
