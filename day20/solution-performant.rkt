#lang racket/base
(require (only-in racket/port port->lines)
         (only-in racket/match match match-define)
         (only-in racket/list drop-right count))
(define grid (list->vector (map (compose list->vector string->list) (with-input-from-file "input.txt" port->lines))))
(define vert (vector-length grid))
(define horz (vector-length (vector-ref grid 0)))
(define (value-at grid x y) (vector-ref (vector-ref grid y) x))
(define (value-at-xy grid xy) (let ([x (real-part xy)] [y (imag-part xy)]) (value-at grid x y)))
(define (find-val grid val)
  (for*/first ([y (in-range vert)] [x (in-range horz)] #:when (char=? (value-at grid x y) val))
    (make-rectangular x y)))
(define (neighbours pt) (list (+ pt 1) (- pt 1) (+ pt +i) (- pt +i)))
(define start (find-val grid #\S))
(define start-dir (for/first ([cand (in-list (neighbours start))]
                              [dir (in-list (list 'e 'w 's 'n))]
                              #:when (char=? (value-at-xy grid cand) #\.))
                    dir))
(define end (find-val grid #\E))
(define (next-point-in-dir pt dir) (+ pt (match dir ['n -i] ['s +i] ['w -1] ['e +1])))
(define (clockwise dir) (match dir ['n 'e] ['e 's] ['s 'w] ['w 'n]))
(define (anticlockwise dir) (match dir ['n 'w] ['w 's] ['s 'e] ['e 'n]))
(define (next-in-track grid pt dir)
  (for/first ([d* (in-list (list dir (clockwise dir) (anticlockwise dir)))]
              #:do [(define p* (next-point-in-dir pt d*))]
              #:when (let ([v* (value-at-xy grid p*)]) (or (char=? v* #\.) (char=? v* #\E))))
    (cons p* d*)))
(struct cheat. (dist pt) #:transparent)
(define (potential-cheats grid pt dist)
  (define-values (px py) (values (real-part pt) (imag-part pt)))
  (for*/list ([x (in-range (max 0 (- px dist)) (min horz (+ 1 px dist)))]
              #:do [(define dx (abs (- px x)))]
              [y (in-range (max 0 (- py (- dist dx)))
                           (min vert (+ 1 py (- dist dx))))]
              #:do [(define v* (value-at grid x y))]
              #:when (or (char=? v* #\.) (char=? v* #\E)))
    (cheat. (+ dx (abs (- py y))) (make-rectangular x y))))

(define point-steps (build-vector vert (λ(_) (build-vector horz (λ(_) 0)))))
(define (set-score pt score) (let ([x (real-part pt)] [y (imag-part pt)])
                               (vector-set! (vector-ref point-steps y) x score)))
(define (get-score pt) (let ([x (real-part pt)] [y (imag-part pt)])
                         (vector-ref (vector-ref point-steps y) x)))
(define race-track
  (let loop ([steps 0]
             [cur-pt start]
             [cur-dir start-dir])
    (if (= cur-pt end)
        (begin0 (list (cons end cur-dir)) (set-score cur-pt steps))
        (let ()
          (match-define (cons p* d*) (next-in-track grid cur-pt cur-dir))
          (set-score cur-pt steps) (cons (cons cur-pt cur-dir)
                                         (loop (add1 steps) p* d*))))))

(define (find-cheats max-time)
  (for/sum ([p+d (in-list (drop-right race-track 100))])
    (match-define (cons p d) p+d)
    (define cheat-pts (potential-cheats grid p max-time))
    (count (λ(dist+cheat)
             (match-define (cheat. dist cheat) dist+cheat)
             (define saved (- (get-score cheat) (get-score p) dist))
             (>= saved 100))
           cheat-pts)))

(for/last ([_ (in-range 1)])
  (find-cheats 2)  #; 1454
  (find-cheats 20) #;997879)
