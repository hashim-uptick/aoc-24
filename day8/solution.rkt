#lang racket

(define inp (map string->list (with-input-from-file "input.txt" port->lines)))

(define (value-at grid at)
  (let ([x (real-part at)] [y (imag-part at)])
    (if (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (list-ref grid 1))))
        #f
        (list-ref (list-ref grid y) x))))

(define antenna-locs
  (for*/fold ([locs (hash)])
             ([x (range (length (first inp)))]
              [y (range (length inp))]
              #:do [(define coord (make-rectangular x y))]
              #:do [(define value (value-at inp coord))]
              #:when (not (equal? value #\.)))
    (hash-update locs value (curry cons coord) '())))

(define (in-bounds? grid coord)
  (define-values (x y) (values (real-part coord) (imag-part coord)))
  (and (>= x 0) (>= y 0) (< x (length (first grid))) (< y (length grid))))

;; part 1

(define (find-antinodes grid coords)
  (for/fold ([antinodes '()])
            ([a&b (combinations coords 2)])
    (match-define (list x1 x2) (map real-part a&b))
    (match-define (list y1 y2) (map imag-part a&b))
    (let* ([x-dist (- x2 x1)]
           [y-dist (- y2 y1)]
           [^x1 (- x1 x-dist)]
           [^x2 (+ x2 x-dist)]
           [^y1 (- y1 y-dist)]
           [^y2 (+ y2 y-dist)]
           [candidate-1 (make-rectangular ^x1 ^y1)]
           [candidate-2 (make-rectangular ^x2 ^y2)])
      (append antinodes (filter (curry in-bounds? grid) (list candidate-1 candidate-2))))))

(define antinodes
  (for/fold ([all-coords '()]
             #:result (length (remove-duplicates all-coords)))
            ([coords (hash-values antenna-locs)])
    (append all-coords (find-antinodes inp coords))))

antinodes

;; part 2

(define (find-antinodes-2 grid coords)
  (define x-lim (length (list-ref grid 0)))
  (define y-lim (length grid))
  (for/fold ([antinodes '()])
            ([a&b (combinations coords 2)])
    (match-define (list x1 x2) (map real-part a&b))
    (match-define (list y1 y2) (map imag-part a&b))
    (define dx (- x2 x1))
    (define dy (- y2 y1))
    (define x-start x1)
    (define x-end (cond [(negative? dx) -1]
                        [(positive? dx) x-lim]
                        [else (add1 x-start)]))
    (define y-start y1)
    (define y-end (cond [(negative? dy) -1]
                        [(positive? dy) x-lim]
                        [else (add1 y-start)]))
    (define half-line
      (for/list ([x (range x1 x-end dx)]
                 [y (range y1 y-end dy)])
        (make-rectangular x y)))
    (define remaining-half-line
      (for/list ([x (range x1 (cond [(negative? dx) x-lim]
                                    [(positive? dx) -1]
                                    [else (add1 x1)]) (* -1 dx))]
                 [y (range y1 (cond [(negative? dy) x-lim]
                                    [(positive? dy) -1]
                                    [else (add1 y1)]) (* -1 dy))])
        (make-rectangular x y)))
    (append antinodes
            half-line
            remaining-half-line)))

(define antinodes-2
  (for/fold ([all-coords '()]
             #:result (length (remove-duplicates all-coords)))
            ([coords (hash-values antenna-locs)])
    (append all-coords (find-antinodes-2 inp coords))))

antinodes-2
