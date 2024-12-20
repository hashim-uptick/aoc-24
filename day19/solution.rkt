#lang racket

(match-define (list pats-s reqs-s) (string-split (string-trim (with-input-from-file "input.txt" port->string)) "\n\n"))

(define pats (map string->list (string-split pats-s ", ")))
(define reqs (map string->list (string-split reqs-s)))
(define max-len (apply max (map length pats)))

(define results (make-hash))
(define (candidates req)
  (define shortened (if (> (length req) max-len) (take req max-len) req))
  (define prev-result (hash-ref results shortened 'nope))
  (if (equal? 'nope prev-result)
      (let ([result (for/list ([p pats]
                               #:when (and (>= (length shortened) (length p))
                                           (equal? (take shortened (length p)) p)))
                      p)])
        (hash-set! results shortened result)
        result)
      prev-result))

(define results2 (make-hash))
(define (possible? req)
  (let loop ([remaining req])
    (define prev-result (hash-ref results2 remaining 'none))
    (if (not (equal? 'none prev-result))
        prev-result
        (let ()
          (if (null? remaining)
              #t
              (let ([cs (candidates remaining)])
                (if (null? cs)
                    (begin0 #f (hash-set! results2 remaining #f))
                    (let ([answer (ormap loop (filter-map (λ(c) (and (not (null? c)) (drop remaining (length c)))) cs))])
                      (hash-set! results2 remaining answer)
                      answer))))))))

(count possible? reqs)


(define results3 (make-hash))
(define (count-ways req)
  (let loop ([remaining req])
    (define prev-result (hash-ref results3 remaining 'none))
    (if (not (equal? 'none prev-result))
        prev-result
        (let ()
          (if (null? remaining)
              1
              (let ([cs (candidates remaining)])
                (if (null? cs)
                    (begin0 0 (hash-set! results3 remaining 0))
                    (let ([answer (for/sum ([c (filter-map (λ(c) (and (not (null? c)) (drop remaining (length c)))) cs)])
                                    (loop c))])
                      (hash-set! results3 remaining answer)
                      answer))))))))
(for/sum ([r reqs]) (count-ways r))
