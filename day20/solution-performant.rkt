#lang racket/base
(require (only-in racket/port port->string)
         (only-in racket/string string-trim)
         (only-in racket/match match)  ;; todo: maybe get rid of this too.
         (only-in racket/list drop-right count)
         (only-in racket/unsafe/ops
                  [unsafe-fx+ u+]
                  [unsafe-fx- u-]
                  [unsafe-fxmax umax]
                  [unsafe-fxmin umin]
                  [unsafe-fxabs uabs]
                  [unsafe-char=? uchar=?]
                  [unsafe-vector-ref uvector-ref]
                  [unsafe-car ucar]
                  [unsafe-cdr ucdr]))
;; todo: parallelise find-cheats .. and maybe race-track too? see shared fx-vector
;; todo: more reduction in startup time .. try and reduce the number of imports.
(define inp (string-trim (with-input-from-file "input.txt" port->string)))
(define-values (l-grid vert horz start end)
  (time
   (for/fold ([grid '()] [row '()] [vert 1] [horz 0] [start 0] [end 0]
                         #:result (values (reverse (cons (reverse row) grid)) vert horz start end))
             ([c (in-string inp)])
     (if (char=? #\newline c)
         (values (cons (reverse row) grid) '() (add1 vert) 0 start end)
         (values grid (cons c row) vert (add1 horz)
                 (if (and (= start 0) (char=? c #\S)) (make-rectangular horz (sub1 vert)) start)
                 (if (and (= end 0) (char=? c #\E)) (make-rectangular horz (sub1 vert)) end))))))
(define grid (list->vector (map list->vector l-grid)))
(define (value-at grid x y) (uvector-ref (uvector-ref grid y) x))
(define (value-at-xy grid xy) (let ([x (real-part xy)] [y (imag-part xy)]) (value-at grid x y)))

(define start-dir (for/first ([cand (in-list (list (+ start 1) (- start 1) (+ start +i) (- start +i)))]
                              [dir (in-list (list 'e 'w 's 'n))]
                              #:when (uchar=? (value-at-xy grid cand) #\.)) dir))

(define (next-point-in-dir pt dir) (+ pt (match dir ['n -i] ['s +i] ['w -1] ['e +1])))
(define (clockwise dir) (match dir ['n 'e] ['e 's] ['s 'w] ['w 'n]))
(define (anticlockwise dir) (match dir ['n 'w] ['w 's] ['s 'e] ['e 'n]))
(define (next-in-track grid pt dir)
  (for/first ([d* (in-list (list dir (clockwise dir) (anticlockwise dir)))]
              #:do [(define p* (next-point-in-dir pt d*))]
              #:when (let ([v* (value-at-xy grid p*)]) (or (uchar=? v* #\.) (uchar=? v* #\E))))
    (cons p* d*)))

(define (potential-cheats grid pt dist pt-score [threshold 100])
  (define-values (px py) (values (real-part pt) (imag-part pt)))
  (for*/sum ([x (in-range (umax 0 (u- px dist)) (umin horz (u+ 1 px dist)))]
              #:do [(define dx (uabs (u- px x)))]
              [y (in-range (umax 0 (u- py (u- dist dx)))
                           (umin vert (u+ 1 py (u- dist dx))))]
              #:do [(define v* (value-at grid x y))]
              #:when (and (or (uchar=? v* #\.) (uchar=? v* #\E))
                          (>= (- (get-score x y) pt-score (u+ dx (uabs (u- py y)))) threshold)))
    1))

#;(define point-steps (build-vector vert (λ(_) (build-vector horz (λ(_) 0))))) ;; doesn't seem to make a difference.
(define point-steps (make-vector (* vert horz) 0))
(define (set-score pt score) (let ([x (real-part pt)] [y (imag-part pt)]) (vector-set! point-steps (+ (* horz y) x) score)))
(define (get-score x y) (uvector-ref point-steps (+ (* horz y) x)))
(define race-track
  (time
   (let loop ([steps 0]
              [cur-pt start]
              [cur-dir start-dir])
     (if (= cur-pt end)
         (begin0 (list end) (set-score cur-pt steps))
         (let ()
           (define p+d* (next-in-track grid cur-pt cur-dir))
           (define-values (p* d*) (values (ucar p+d*) (ucdr p+d*)))
           (set-score cur-pt steps) (cons cur-pt (loop (add1 steps) p* d*)))))))

(define (find-cheats max-time)
  (for/sum ([p (in-list (drop-right race-track 100))])
    (define p-score (let ([x (real-part p)] [y (imag-part p)]) (get-score x y)))
    (potential-cheats grid p max-time p-score)))

(time (find-cheats 2))  #; 1454
(time (find-cheats 20)) #;997879
