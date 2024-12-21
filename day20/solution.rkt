#lang racket
(define grid (map string->list (with-input-from-file "input.txt" port->lines)))
(define vert (length grid))
(define horz (length (first grid)))
(define (value-at grid x y) (and (>= x 0) (>= y 0) (< y vert) (< x horz)
                                 (list-ref (list-ref grid y) x)))
(define (value-at-xy grid xy) (let ([x (real-part xy)] [y (imag-part xy)]) (value-at grid x y)))
(define (find-val grid val)
  (for*/first ([y (range vert)] [x (range horz)] #:when (char=? (value-at grid x y) val))
    (make-rectangular x y)))
(define (neighbours pt) (list (+ pt 1) (- pt 1) (+ pt +i) (- pt +i)))
(define start (find-val grid #\S))
(define start-dir (for/first ([cand (neighbours start)]
                              [dir (list 'e 'w 's 'n)]
                              #:when (char=? (value-at-xy grid cand) #\.))
                    dir))
(define end (find-val grid #\E))
(define (next-point-in-dir pt dir) (+ pt (match dir ['n -i] ['s +i] ['w -1] ['e +1])))
(define (clockwise dir) (match dir ['n 'e] ['e 's] ['s 'w] ['w 'n]))
(define (anticlockwise dir) (match dir ['n 'w] ['w 's] ['s 'e] ['e 'n]))
(define (next-in-track grid pt dir)
  (for/first ([d* (list dir (clockwise dir) (anticlockwise dir))]
              #:do [(define p* (next-point-in-dir pt d*))]
              #:when (member (value-at-xy grid p*) (list #\. #\E)))
    (cons p* d*)))

(define (potential-cheats grid pt dist)
  (remove-duplicates
   (for*/list ([x (in-range 0 (add1 dist))]
               [yn (in-range 0 (add1 (- dist x)))]
               #:do [(define y (make-rectangular 0 yn))]
               [dx (list x (- x))]
               [dy (list y (- y))]
               #:do [(define p* (+ pt dx dy)) (define v* (value-at-xy grid p*))]
               #:when (member v* '(#\. #\E)))
     (cons (+ x yn) p*))))

(define point-steps (make-hash))
(define race-track
  (let loop ([steps 0]
             [cur-pt start]
             [cur-dir start-dir])
    (if (equal? cur-pt end)
        (begin0 (list (cons end cur-dir)) (hash-set! point-steps cur-pt steps))
        (let ()
          (match-define (cons p* d*) (next-in-track grid cur-pt cur-dir))
          (hash-set! point-steps cur-pt steps) (cons (cons cur-pt cur-dir)
                                                     (loop (add1 steps) p* d*))))))

(define (find-cheats max-time)
  (for/sum ([p+d race-track])
  (match-define (cons p d) p+d)
  (define cheat-pts (potential-cheats grid p max-time))
  (define good-cheats
    (filter-map (λ(dist+cheat)
                   (match-define (cons dist cheat) dist+cheat)
                   (define saved (- (hash-ref point-steps cheat) (hash-ref point-steps p) dist))
                   (and (>= saved 100 ) (cons cheat saved)))
                 cheat-pts))
  (length good-cheats)))

(find-cheats 2)  ;; 1454
(find-cheats 20)  ;; 997879
