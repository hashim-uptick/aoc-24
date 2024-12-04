#lang racket

(define grid (map string->list (with-input-from-file "input.txt" port->lines)))

(define (point-at grid at)
  (let ([x (real-part at)] [y (imag-part at)])
    (if (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (list-ref grid 1))))
        #f
        (list-ref (list-ref grid y) x))))

(define (take-n at grid dir n)
  (define this-point (point-at grid at))
  (cond [(or (= n 0) (not this-point)) '()]
        [else
         (cons this-point
               (match dir
                 ["n" (take-n (+ at 0-1i) grid dir (sub1 n))]
                 ["s" (take-n (+ at 0+1i) grid dir (sub1 n))]
                 ["e" (take-n (+ at 1+0i) grid dir (sub1 n))]
                 ["w" (take-n (+ at -1+0i) grid dir (sub1 n))]
                 ["nw" (take-n (+ at -1-1i) grid dir (sub1 n))]
                 ["ne" (take-n (+ at 1-1i) grid dir (sub1 n))]
                 ["sw" (take-n (+ at -1+1i) grid dir (sub1 n))]
                 ["se" (take-n (+ at 1+1i) grid dir (sub1 n))]))]))

(define (take-n-string at grid dir n)
  (list->string (take-n at grid dir n)))

;; part 1
(let ([directions '("n" "s" "w" "e" "nw" "ne" "sw" "se")])
  (for*/sum ([x (in-range (length (list-ref grid 1)))]
             [y (in-range (length grid))])
    (let ([at (make-rectangular x y)])
      (for/sum ([dir (in-list directions)])
        (if (equal? (take-n-string at grid dir 4) "XMAS")
            1
            0)))))

;; part 2
(for*/sum ([x (in-range (length (list-ref grid 1)))]
           [y (in-range (length grid))])
  (let ([at (make-rectangular x y)])
    (if (and (equal? (point-at grid at) #\A)
             (let ([diag-1 (take-n-string (+ at -1-i) grid "se" 3)]
                   [diag-2 (take-n-string (+ at 1-i) grid "sw" 3)])
               (and (regexp-match #rx"(SAM|MAS)" diag-1)
                    (regexp-match #rx"(SAM|MAS)" diag-2))))
        1
        0)))


