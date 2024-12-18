#lang racket

;; prints 2161 -- answer is too high.

(require (only-in racket/control shift reset))

(define grid (map string->list (with-input-from-file "input.txt" port->lines)))

(define (value-at grid at)
  (let ([x (real-part at)] [y (imag-part at)])
    (if (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (list-ref grid 1))))
        #f
        (list-ref (list-ref grid y) x))))

(define guard-loc  ;; 70+59i
  (for*/first ([x (in-range (length (list-ref grid 1)))]
               [y (in-range (length grid))]
               #:do [(define pt (make-rectangular x y))]
               #:when (equal? (value-at grid pt) #\^))
    pt))

(define (move-point-in-dir point dir)
  (match dir
    ['up (+ point 0-1i)]
    ['down (+ point 0+1i)]
    ['left (+ point -1+0i)]
    ['right (+ point 1+0i)]))

(define (rotate dir)
  (match dir
    ['up 'right]
    ['right 'down]
    ['down 'left]
    ['left 'up]))

(struct moving (pt dir) #:transparent)
 
;; part 1
(define (run grid [guard-loc guard-loc] [dir 'up] [visited (set)])
  (define next-point (move-point-in-dir guard-loc dir))
  (define next-val (value-at grid next-point))
  (match next-val
    [#f (set-add visited (moving guard-loc dir))]
    [#\# (let ([new-dir (rotate dir)])
           (run grid guard-loc new-dir (set-add visited (moving guard-loc dir))))]
    [#\^ (if (equal? dir 'up)
             (shift k #f)
             (if (set-member? visited (moving next-point dir))
                 (shift k #f)
                 (run grid next-point dir (set-add visited (moving guard-loc dir)))))]
    [#\. (if (set-member? visited (moving next-point dir))
             (shift k #f)
             (run grid next-point dir (set-add visited (moving guard-loc dir))))]))

(define visited-points (run grid))
(length (remove-duplicates (map moving-pt (set->list visited-points))))

;; part2

(define (add-obstacle-at grid pt)
  (define-values (x y) (values (real-part pt) (imag-part pt)))
  (list-update grid y (λ(row) (list-update row x (λ(v) #\#)))))

(define (simulate grid [guard-loc guard-loc] [dir 'up] [visited (set)] [loops 0])
  (define next-point (move-point-in-dir guard-loc dir))
  (define next-val (value-at grid next-point))
  (match next-val
    [#f loops]
    [#\# (let ([new-dir (rotate dir)])
           (simulate grid guard-loc new-dir (set-add visited (moving guard-loc dir)) loops))]
    [#\. (let ([visited^ (set-add visited (moving guard-loc dir))])
           (if (not (reset (run (add-obstacle-at grid next-point) guard-loc dir visited^)))
               (simulate grid next-point dir visited^ (add1 loops))
               (simulate grid next-point dir visited^ loops)))]))

(simulate grid)