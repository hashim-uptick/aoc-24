#lang racket
(require racket/gui/base)

(define inp (with-input-from-file "input.txt" port->lines))
(define data (map (compose (curry map string->number) (curry regexp-match* #px"-?\\d+")) inp))
(define-values (width height) (values 101 103))

(define (calc-posn-at t robot)
  (define x (+ (first robot) (* t (third robot))))
  (define y (+ (second robot) (* t (fourth robot))))
  (define x^ (if (>= x 0) (remainder x width) (remainder (- width (remainder (abs x) width)) width)))
  (define y^ (if (>= y 0) (remainder y height) (remainder (- height (remainder (abs y) height)) height)))
  (cons x^ y^))

(define (find-quad posn)
  (match-define (cons x y) posn)
  (define-values (x-mid y-mid) (values (/ (sub1 width) 2) (/ (sub1 height) 2)))
  (cond [(and (= x x-mid) (= y y-mid)) #f]
        [(and (< x x-mid) (< y y-mid)) 'first]
        [(and (> x x-mid) (< y y-mid)) 'second]
        [(and (< x x-mid) (> y y-mid)) 'third]
        [(and (> x x-mid) (> y y-mid)) 'fourth]))

(define posns
  (map (curry calc-posn-at 100) data))

;; part 1

(define quads (filter-map find-quad posns))

(for/product ([quad '(first second third fourth)])
  (count (curry equal? quad) quads))

;; part 2

;; 7344
(for/first ([n (in-naturals)]
            #:do [(define posns (map (curry calc-posn-at n) data))]
            #:when (= (length posns) (length (remove-duplicates posns))))
  n)

;; Ignore everything below.

(define (posns->lines posns)
  (define posn-set (list->set posns))
  (string-join (for/list ([y (range height)])
                 (list->string
                  (for/list ([x (range width)])
                    (if (set-member? posn-set (cons x y)) #\# #\.)))) "\n"))

(define frame (new frame% [label "Visualiser"]))

(define parent-panel (new vertical-panel%
                          (parent frame)
                          (style (list 'border))))

(define control-panel (new horizontal-panel%
                           (parent parent-panel)
                           (style (list 'border))
                           (stretchable-height #f)))

(define less-button (new button%
                         (parent control-panel)
                         (label "Decrease")
                         (callback (λ(b e)
                                     (define time (string->number (send time-label get-label)))
                                     (define time^ (abs (sub1 time)))
                                     (send time-label set-label (number->string time^))
                                     (send result-text-area
                                           set-value (posns->lines (map (curry calc-posn-at time^) data)))))))

(define time-label (new message%
                        (parent control-panel)
                        (label "0")
                        (min-width 50)))

(define more-button (new button%
                         (parent control-panel)
                         (label "Increase")
                         (callback (λ(b e)
                                     (define time (string->number (send time-label get-label)))
                                     (define time^ (add1 time))
                                     (send time-label set-label (number->string time^))
                                     (send result-text-area
                                           set-value (posns->lines (map (curry calc-posn-at time^) data)))))))

(define start-button (new button%
                          (parent control-panel)
                          (label "Start")
                          (callback (λ(b e) (start (string->number (send time-label get-label)))))))

(define stop-button (new button%
                         (parent control-panel)
                         (label "Stop")
                         (callback (λ(b e) (stop)))))

(define result-panel (new panel%
                          (parent parent-panel)
                          (style (list 'border))))

(define result-text-area
  (new text-field%
       (parent result-panel)
       (label "Result")
       (init-value (posns->lines (map (curry calc-posn-at 0) data)))
       (style '(multiple))
       (font (send the-font-list find-or-create-font 7 'modern 'normal 'normal))
       (stretchable-width #t)
       (stretchable-height #t)))

(send frame show #t)

(define t #f)
(define (start n-start)
  (set! t
        (thread
         (thunk (for ([n (in-naturals n-start)])
                  (sleep 0.02)
                  (send time-label set-label (number->string n))
                  (send result-text-area
                        set-value (posns->lines (map (curry calc-posn-at n) data))))))))
(define (stop) (kill-thread t) (set! t #f))
