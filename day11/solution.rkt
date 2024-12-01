#lang racket

(define inp (string-split (string-trim (with-input-from-file "input.txt" port->string))))

(define (round-trip-str-num str)
  (number->string (string->number str)))

;; part 1

(define (blink stones)
  (let loop ([remaining stones])
    (if (null? remaining)
        '()
        (let* ([remaining^ (rest remaining)]
               [this (first remaining)]
               [this-size (string-length this)]
               [half-size (/ this-size 2)])
          (cond
            [(equal? this "0")
             (cons "1" (loop remaining^))]
            [(even? this-size)
             (cons (round-trip-str-num (substring this 0 half-size))
                   (cons (round-trip-str-num (substring this half-size this-size))
                         (loop remaining^)))]
            [else
             (cons (number->string (* 2024 (string->number this)))
                   (loop remaining^))])))))

(length  ;; 191690
 (for/fold ([state inp])
           ([i (range 25)])
   (blink state)))

;; part 2

(define cache (make-hash))

(define (makes-stones stone n)
  (define cached (hash-ref cache (cons stone n) #f))
  (or cached
      (let ([result
             (cond [(= 0 n) 1]
                   [(equal? "0" stone)
                    (makes-stones "1" (sub1 n))]
                   [else
                    (let* ([this-size (string-length stone)]
                           [half-size (/ this-size 2)])
                      (if (even? this-size)
                          (+ (makes-stones (round-trip-str-num (substring stone 0 half-size)) (sub1 n))
                             (makes-stones (round-trip-str-num (substring stone half-size this-size)) (sub1 n)))
                          (makes-stones (number->string (* 2024 (string->number stone))) (sub1 n))))])])
        (hash-set! cache (cons stone n) result)
        result)))

(for/sum ([stone inp])
  (makes-stones stone 75)) ;; 228651922369703
