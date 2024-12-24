#lang racket/base
(require (only-in racket/port port->string)
         (only-in racket/string string-trim)
         (only-in racket/match match)
         (only-in racket/future future touch)
         (only-in racket/unsafe/ops
                  [unsafe-fx+ u+]
                  [unsafe-fx- u-]
                  [unsafe-fx* u*]
                  [unsafe-fxmax umax]
                  [unsafe-fxmin umin]
                  [unsafe-fxabs uabs]
                  [unsafe-char=? uchar=?]
                  [unsafe-vector-ref uvector-ref]
                  [unsafe-car ucar]
                  [unsafe-cdr ucdr]
                  [unsafe-vector-set! uvector-set!]))

(define inp (string-trim (with-input-from-file "input.txt" port->string)))
(define-values (l-grid vert horz start-c end-c)
  (for/fold ([grid '()] [row '()] [vert 1] [horz 0] [start #f] [end #f]
                        #:result (values (reverse (cons (reverse row) grid)) vert horz start end))
            ([c (in-string inp)])
    (if (uchar=? #\newline c)
        (values (cons (reverse row) grid) '() (u+ 1 vert) 0 start end)
        (values grid (cons c row) vert (u+ 1 horz)
                (if (and (not start) (uchar=? c #\S)) (cons horz (u- vert 1)) start)
                (if (and (not end) (uchar=? c #\E)) (cons horz (u- vert 1)) end)))))
(define grid (list->vector (apply append l-grid)))
(define (value-at grid x y) (uvector-ref grid (u+ x (u* y horz))))
(define (value-at-xy grid xy) (uvector-ref grid xy))
(define start (u+ (ucar start-c) (u* horz (ucdr start-c))))
(define end (u+ (ucar end-c) (u* horz (ucdr end-c))))
(define start-dir (for/first ([cand (in-list (list (u+ start 1) (u- start 1) (u+ start horz) (u- start horz)))]
                              [dir (in-list (list 'e 'w 's 'n))]
                              #:when (uchar=? (value-at-xy grid cand) #\.)) dir))

(define (next-point-in-dir pt dir) (u+ pt (match dir ['n (u- horz)] ['s horz] ['w -1] ['e +1])))
(define (clockwise dir) (match dir ['n 'e] ['e 's] ['s 'w] ['w 'n]))
(define (anticlockwise dir) (match dir ['n 'w] ['w 's] ['s 'e] ['e 'n]))
(define (next-in-track grid pt dir)
  (let loop ([remaining (list dir (clockwise dir) (anticlockwise dir))])
    (define p* (next-point-in-dir pt (ucar remaining)))
    (if (let ([v* (value-at-xy grid p*)]) (or (uchar=? v* #\.) (uchar=? v* #\E)))
        (values p* (ucar remaining))
        (loop (ucdr remaining)))))

(define (potential-cheats grid pt dist pt-score [threshold 100])
  (define-values (px py) (values (remainder pt horz) (quotient pt horz)))
  (for*/sum ([x (in-range (umax 0 (u- px dist)) (umin horz (u+ 1 px dist)))]
             #:do [(define dx (uabs (u- px x)))]
             [y (in-range (umax 0 (u- py (u- dist dx)))
                          (umin vert (u+ 1 py (u- dist dx))))]
             #:do [(define v* (value-at grid x y))]
             #:when (and (or (uchar=? v* #\.) (uchar=? v* #\E))
                         (>= (u- (get-score x y) pt-score (u+ dx (uabs (u- py y)))) threshold)))
    1))

(define point-steps (make-vector (u* vert horz) 0))
(define (set-score pt score) (let () (uvector-set! point-steps pt score)))
(define (get-score x y) (uvector-ref point-steps (u+ (u* horz y) x)))
(define (get-score-xy xy) (uvector-ref point-steps xy))
(define race-track (make-vector (quotient (u* vert horz) 2) 0))  ;; over-pre-allocate space, we'll know the length next
(define race-track-len
  (let loop ([steps 0]
             [cur-pt start]
             [cur-dir start-dir])
    (if (= cur-pt end)
        (begin0 (add1 steps) (uvector-set! race-track steps end) (set-score cur-pt steps))
        (let ()
          (define-values (p* d*) (next-in-track grid cur-pt cur-dir))
          (set-score cur-pt steps)
          (uvector-set! race-track steps cur-pt)
          (loop (u+ 1 steps) p* d*)))))

(define (find-cheats max-time)  ;; Is this really slower?
  (define (inner from to)
    (for/sum ([p (in-vector race-track from to)])
      (define p-score (get-score-xy p))
      (potential-cheats grid p max-time p-score)))
  (define chunk-size (u+ 1 (quotient (u- race-track-len 100) 8)))
  (define tasks
    (let loop ([idx 0])
      (if (>= idx (u- race-track-len 100))
          '()
          (cons (future (λ() (inner idx (u+ idx chunk-size)))) (loop (u+ idx chunk-size))))))
  (apply + (map touch tasks)))

(define (find-cheats-seq max-time)
  (for/sum ([p (in-vector race-track 0 (u- race-track-len 100))])
    (define p-score (get-score-xy p))
    (potential-cheats grid p max-time p-score)))

#;(find-cheats-seq 2)  #; 1454
#;(find-cheats-seq 20) #;997879
(find-cheats 2)
(find-cheats 20)
