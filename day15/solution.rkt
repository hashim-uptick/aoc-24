#lang racket

(match-define (list whs dirs) (string-split (string-trim (with-input-from-file "input.txt" port->string)) "\n\n"))

(define grid (map string->list (string-split whs)))

(define (value-at grid x y)
  (if (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (list-ref grid 1))))
      #f
      (list-ref (list-ref grid y) x)))

(define (value-at-pt grid pt)
  (value-at grid (real-part pt) (imag-part pt)))

(define (set-value-at grid pt val)
  (when (not (char? val)) (raise-argument-error 'val "char" val))
  (let ([x (real-part pt)]
        [y (imag-part pt)])
    (list-set grid y (let ([row (list-ref grid y)]) (list-set row x val)))))

(define (swap-values-at grid p1 p2)
  (let ([val1 (value-at grid (real-part p1) (imag-part p1))]
        [val2 (value-at grid (real-part p2) (imag-part p2))])
    (define grid^ (set-value-at grid p2 val1))
    (set-value-at grid^ p1 val2)))

(define starting (for*/first ([y (length grid)]
                              [x (length (list-ref grid 0))]
                              #:when (equal? (value-at grid x y) #\@))
                   (make-rectangular x y)))

(define (next-pt-in-dir loc dir)
  (match dir [#\v (+ loc +i)] [#\^ (+ loc -i)] [#\> (+ loc 1)] [#\< (+ loc -1)]))

(define (list-delete lst i)
  (append (take lst i) (drop lst (add1 i))))

(define (list-insert lst i val)
  (append (take lst i) (cons val (drop lst i))))

(define (move grid loc dir)
  (define dir-loc (next-pt-in-dir loc dir))
  (define dir-val (value-at-pt grid dir-loc))
  (define loc-val (value-at-pt grid loc))
  (match dir-val
    [#\# (values grid loc)]
    [#\. (values (swap-values-at grid loc dir-loc) dir-loc)]
    [#\O
     (define free-space
       (let loop ([next-loc (next-pt-in-dir dir-loc dir)])
         (match (value-at-pt grid next-loc)
           [#\# #f]
           [#\. next-loc]
           [#\O (loop (next-pt-in-dir next-loc dir))])))
     (if (not free-space)
         (values grid loc)
         (values (swap-values-at (swap-values-at grid free-space dir-loc) dir-loc loc)
                 dir-loc))]))

(define-values (final-grid final-loc)
  (for/fold ([grid^ grid]
             [loc starting])
            ([dir (filter-not (curry equal? #\newline) (string->list dirs))]
             [n (in-naturals)])
    (move grid^ loc dir)))

;; part 1 ;; 1527563

(for*/sum ([y (length final-grid)]
           [x (length (list-ref final-grid 0))]
           #:when (equal? (value-at final-grid x y) #\O))
  (+ x (* 100 y)))

;; part 2 ;; 1521635

(define grid-2 (map string->list
                    (string-split (string-replace (string-replace (string-replace (string-replace whs "#" "##")
                                                                                  "O" "[]")
                                                                  "." "..")
                                                  "@" "@."))))

(define starting-2 (for*/first ([y (length grid-2)]
                                [x (length (list-ref grid-2 0))]
                                #:when (equal? (value-at grid-2 x y) #\@))
                     (make-rectangular x y)))

(define (move-box grid bl br dir)
  (match dir
    [(or #\< #\>)
     (define free-space
       (let loop ([next-loc (next-pt-in-dir bl dir)])
         (match (value-at-pt grid next-loc)
           [#\# #f]
           [#\. next-loc]
           [(or #\] #\[) (loop (next-pt-in-dir next-loc dir))])))
     (if (not free-space)
         (values grid #f)
         (values
          (let* ([y (imag-part bl)]
                 [row (list-ref grid (imag-part bl))]
                 [row^ (list-delete row (real-part free-space)) ]
                 [row^^ (list-insert row^
                                     (match dir
                                       [#\> (real-part bl)]
                                       [#\< (real-part br)])
                                     #\.)])
            (list-set grid y row^^))
          #t))]
    [else (let loop ([points-to-move (list (list bl br))])
            (define next-points (map (λ(p) (next-pt-in-dir p dir)) (first points-to-move)))
            (if (ormap (λ(p) (equal? #\# (value-at-pt grid p))) next-points)
                (values grid #f)
                (let ()
                  (if (andmap (λ(p) (equal? #\. (value-at-pt grid p))) next-points)
                      (values (for/fold ([g^ grid]) ([p (flatten points-to-move)])
                                (swap-values-at g^ p (next-pt-in-dir p dir)))
                              #t)
                      (let ()
                        (define next-points^ (filter (λ(p) (member (value-at-pt grid p) (list #\[ #\]))) next-points))
                        (define next-points^^
                          (let inner ([remaining next-points^])
                            (if (null? remaining)
                                '()
                                (let* ([cmp (first remaining)]
                                       [cmp-val (value-at-pt grid cmp)])
                                  (cond
                                    [(equal? cmp-val #\])
                                     (cons (- cmp 1) (cons cmp (inner (rest remaining))))]
                                    [(equal? cmp-val #\[)
                                     (if (or (null? (rest remaining))
                                             (not (= 1 (- (real-part (second remaining)) (real-part cmp)))))
                                         (cons cmp (cons (+ 1 cmp) (inner (rest remaining))))
                                         (cons cmp (cons (second remaining) (inner (rest (rest remaining))))))]
                                    [else (error "heh?")])))))
                        (loop (cons next-points^^ points-to-move)))))))]))

(define (move-2 grid loc dir)
  (define dir-loc (next-pt-in-dir loc dir))
  (define dir-val (value-at-pt grid dir-loc))
  (define loc-val (value-at-pt grid loc))
  (match dir-val
    [#\# (values grid loc)]
    [#\. (values (swap-values-at grid loc dir-loc) dir-loc)]
    [(or #\[ #\])
     (define bl (if (equal? dir-val #\[) dir-loc (+ dir-loc -1)))
     (define br (if (equal? dir-val #\[) (+ dir-loc 1) dir-loc))
     (define-values (new-g succeeded) (move-box grid bl br dir))
     (if (not succeeded)
         (values grid loc)
         (values (swap-values-at new-g loc dir-loc) dir-loc))]))

(define-values (final-grid-2 final-loc-2)
  (for/fold ([grid^ grid-2]
             [loc starting-2])
            ([dir (filter-not (curry equal? #\newline) (string->list dirs))]
             [n (in-naturals)])
    (define-values (grid^^ loc^) (move-2 grid^ loc dir))
    (values grid^^ loc^)))

(for*/sum ([y (length final-grid-2)]
           [x (length (list-ref final-grid-2 0))]
           #:when (equal? (value-at final-grid-2 x y) #\[))
  (+ x (* 100 y)))
