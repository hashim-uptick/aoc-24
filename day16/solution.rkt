#lang racket

(require data/heap)
(define maze (map string->list (with-input-from-file "input.txt" port->lines)))

(define (value-at maze pt) (let ([x (real-part pt)] [y (imag-part pt)]) (value-at-xy maze x y)))
(define (value-at-xy maze x y) (list-ref (list-ref maze y) x))
(define (clockwise dir) (match dir ['n 'e] ['e 's] ['s 'w] ['w 'n]))
(define (anti-clockwise dir) (match dir ['n 'w] ['w 's] ['s 'e] ['e 'n]))
(define (next-pt-in-dir pt dir) (+ pt (match dir ['n -i] ['s +i] ['w -1] ['e +1])))
(define (moveable-pt? maze pt) (match (value-at maze pt) [#f #f] [#\# #f] [_ #t]))
(define (possible-moves maze pt dir) (for/list ([d^ (list (anti-clockwise dir) dir (clockwise dir))]
                                                [c (list 1001 1 1001)]
                                                #:do [(define p^ (next-pt-in-dir pt d^))]
                                                #:when (moveable-pt? maze p^)) (cons p^ (cons d^ c))))
(define (find-point which)
  (for*/first ([y (range (length maze))] [x (range (length (first maze)))]
                                         #:do [(define val (value-at-xy maze x y))] #:when (equal? val which))
    (make-rectangular x y)))

(define start-point (find-point #\S))
(define end-point (find-point #\E))

(define (least-cost-path maze start end)  ;; 105496, 524
  (struct node (loc cost dir path) #:transparent)
  (define visited (mutable-set))
  (define fwds (make-heap (λ(x y) (<= (node-cost x) (node-cost y)))))
  (heap-add! fwds (node start 0 'e '()))
  (define (iter)
    (let* ([min-next (begin0 (heap-min fwds) (heap-remove-min! fwds))]
           [cost (node-cost min-next)]
           [same-cost-next (for/list ([_ (in-range (heap-count fwds))] #:when (= (node-cost (heap-min fwds)) cost))
                             (begin0 (heap-min fwds) (heap-remove-min! fwds)))]
           [to-check (cons min-next same-cost-next)])
      (if (ormap (λ(x) (= end (node-loc x))) to-check)
          (cons cost (add1 (set-count (apply set-union (map (compose list->set node-path)
                                                            (filter (λ(x) (= end (node-loc x))) to-check))))))
          (begin (for ([x (in-list to-check)])
                   (set-add! visited (cons (node-loc x) (node-dir x)))
                   (apply heap-add! fwds
                          (for/list ([move (in-list (possible-moves maze (node-loc x) (node-dir x)))]
                                     #:do [(match-define (cons p* (cons d* c*)) move)]
                                     #:when (not (set-member? visited (cons p* d*))))
                            (node p* (+ cost c*) d* (cons (node-loc x) (node-path x))))))
                 (iter)))))
  (iter))

(least-cost-path maze start-point end-point)
