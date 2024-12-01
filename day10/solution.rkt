#lang racket

(define grid (map (curry map (compose string->number string))
                  (map string->list (with-input-from-file "input.txt" port->lines))))

(define (value-at grid at)
  (let ([x (real-part at)] [y (imag-part at)])
    (if (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (list-ref grid 1))))
        #f
        (list-ref (list-ref grid y) x))))

;; part 1

(define 9-locs
  (for*/list ([y (range (length grid))]
              [x (range (length (first grid)))]
              #:do [(define pt (make-rectangular x y))]
              #:when (= 9 (value-at grid pt)))
    pt))

(define (descend grid pt)
  (define current-value (value-at grid pt))
  (define candidate-locs (list (+ pt 1+0i)
                               (+ pt -1+0i)
                               (+ pt 0+1i)
                               (+ pt 0-1i)))
  (define candidate-values (map (curry value-at grid) candidate-locs))
  (define result
    (or (for/list ([loc candidate-locs]
                   [val candidate-values]
                   #:when (and val (= val (sub1 current-value))))
          (if (= val 0)
              loc
              (descend grid loc)))
        '()))
  (flatten result))

(define (run-1)
  (for/fold ([trail-heads (hash)]
             #:result (apply + (hash-values trail-heads)))
            ([peak 9-locs])
    (for/fold ([state trail-heads])
              ([head-loc (remove-duplicates (descend grid peak))])
      (hash-update state head-loc add1 0))))

(run-1)

;; part 2

(define 0-locs
  (for*/list ([y (range (length grid))]
              [x (range (length (first grid)))]
              #:do [(define pt (make-rectangular x y))]
              #:when (= 0 (value-at grid pt)))
    pt))

(define (ascend grid pt)
  (define current-value (value-at grid pt))
  (define candidate-locs (list (+ pt 1+0i)
                               (+ pt -1+0i)
                               (+ pt 0+1i)
                               (+ pt 0-1i)))
  (define candidate-values (map (curry value-at grid) candidate-locs))
  (or (for/sum ([loc candidate-locs]
                [val candidate-values]
                #:when (and val (= val (add1 current-value))))
        (if (= val 9)
            1
            (ascend grid loc)))
      0))

(define (run-2)
  (for/sum ([loc 0-locs])
    (ascend grid loc)))

(run-2)
