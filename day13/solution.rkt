#lang racket
(require math)

(define inp-groups (string-split (with-input-from-file "input.txt" port->string) "\n\n"))
(define probs (map (λ(s) (map string->number (regexp-match* #px"\\d+" s))) inp-groups))

(define (solve g [limit-100? #t])
  (define M (matrix [[(first g) (third g)]
                     [(second g) (fourth g)]]))
  (define B (col-matrix [(fifth g) (sixth g)]))
  (define solution (matrix-solve M B))
  (and solution
       (let ([sol-lst (array->list solution)])
         (and (andmap integer? sol-lst)
              (or (not limit-100?) (andmap (λ(n) (<= n 100)) sol-lst))
              sol-lst))))

;; part 1  ;; 39748
(for/sum ([prob probs]
          #:do [(define soln (solve prob))]
          #:when soln)
  (+ (* 3 (first soln)) (second soln)))

;; part 2  ;; 74478585072604
(for/sum ([prob (map (λ(g) (append (take g 4) (map (curry + 10000000000000) (drop g 4)))) probs)]
          #:do [(define soln (solve prob #f))]
          #:when soln)
  (+ (* 3 (first soln)) (second soln)))
