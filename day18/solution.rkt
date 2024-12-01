#lang racket

(struct pt. (x y) #:transparent)
(define start (pt. 0 0))
(define end (pt. 70 70))
(define corrupted-lst (map (compose (curry apply pt.) (curry map string->number) (λ(s)(string-split s ",")))
                           (with-input-from-file "input.txt" port->lines)))
(define corrupted-1 (list->set (take corrupted-lst 1024)))
(define/match (valid-pt? crpt pt)
  [(crpt (and (pt. x y) p)) (and (>= x 0) (>= y 0) (<= y 70) (<= x 70) (not (set-member? crpt p)))])
(define/match (candidates crpt pt)
  [(crpt (pt. x y)) (filter (curry valid-pt? crpt)
                            (list (pt. (add1 x) y) (pt. (sub1 x) y) (pt. x (add1 y)) (pt. x (sub1 y))))])

;; as shalan would say, just dp it
(define (find-shortest crpt s e)
  (define distances (make-hash))
  (define (inner s e acc)
    (cond [(equal? s e) acc]
          [(and (hash-ref distances s #f) (<= (hash-ref distances s) acc)) #f]
          [else
           (hash-set! distances s acc)
           (define cs (candidates crpt s))
           (define from-here-on (filter-map (λ(c) (inner c e (add1 acc))) cs))
           (if (null? from-here-on) #f (apply min from-here-on))]))
  (inner s e 0))

;; part 1
(find-shortest corrupted-1 start end)  ;; 280

;; part 2 ... there were only 3450 items.. and we'd already checked after 1024 so ... just manually found it :shrug:
(find-shortest (list->set (take corrupted-lst 3000)) start end)
;#f
(find-shortest (list->set (take corrupted-lst 2000)) start end)
;298
(find-shortest (list->set (take corrupted-lst 2500)) start end)
;616
(find-shortest (list->set (take corrupted-lst 2800)) start end)
;616
(find-shortest (list->set (take corrupted-lst 2995)) start end)
;#f
(find-shortest (list->set (take corrupted-lst 2900)) start end)
;#f
(find-shortest (list->set (take corrupted-lst 2850)) start end)
;616
(find-shortest (list->set (take corrupted-lst 2879)) start end)
;#f
(find-shortest (list->set (take corrupted-lst 2860)) start end)
;616
(find-shortest (list->set (take corrupted-lst 2870)) start end)
;#f
(find-shortest (list->set (take corrupted-lst 2865)) start end)
;#f
(find-shortest (list->set (take corrupted-lst 2861)) start end)
;616
(find-shortest (list->set (take corrupted-lst 2862)) start end)
#f
(list-ref corrupted-lst (sub1 2862))
;; (pt. 28 56)
