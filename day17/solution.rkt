#lang racket
(require (only-in racket/control shift reset))

(define inp (string-split (with-input-from-file "input.txt" port->string) "\n\n"))

(match-define (list A B C) (map string->number (regexp-match* #px"\\d+" (first inp))))
(define program (map string->number (regexp-match* #px"\\d+" (second inp))))

(define/match (parse-op x)
  [(4) A] [(5) B] [(6) C] [(7) (error "7")] [(n) n])

(define ptr 0)
(define insts
  (list (λ(x) (set! A (truncate (/ A (expt 2 (parse-op x))))) (set! ptr (+ 2 ptr)))
        (λ(x) (set! B (bitwise-xor B x)) (set! ptr (+ 2 ptr)))
        (λ(x) (set! B (modulo (parse-op x) 8)) (set! ptr (+ 2 ptr)))
        (λ(x) (if (= A 0) (set! ptr (+ 2 ptr)) (set! ptr x)))
        (λ(x) (set! B (bitwise-xor B C)) (set! ptr (+ 2 ptr)))
        (λ(x) (display (modulo (parse-op x) 8)) (display ",") (set! ptr (+ 2 ptr)))
        (λ(x) (set! B (truncate (/ A (expt 2 (parse-op x))))) (set! ptr (+ 2 ptr)))
        (λ(x) (set! C (truncate (/ A (expt 2 (parse-op x))))) (set! ptr (+ 2 ptr)))))

(define (run-program insts)
  (let loop ()
    (if (>= ptr (length program))
        (void)
        (let ([inst (list-ref insts (list-ref program ptr))]
              [op (list-ref program (add1 ptr))])
          (inst op)
          (loop)))))

(run-program insts)  ;; 3,1,5,3,7,4,2,7,5, (eh just cut the last comma when pasting)

(define (reset-with-A-to n) (set! A n) (set! B 0) (set! C 0) (set! ptr 0))

;; So I was kinda -- to think of my self charitably -- close, but I was considering
;; digits 1 by 1, from first to last.
;; But actually one has to consider all the digits together, from last to first,
;; credit to this guy: https://old.reddit.com/r/adventofcode/comments/1hg38ah/2024_day_17_solutions/m2gge90/
;; Crucially, though, it needs to be a backtracking search. Yay, elegant use of shift/reset!

(let loop ([matching 1]  ;; 190593310997519
           [result 0])
  (if (> matching 16)
      result
      (let inner ([m 0])
        (reset-with-A-to (+ m (* 8 result)))
        (define digits (map string->number (string-split (with-output-to-string (thunk (run-program insts))) ",")))
        (cond [(> m 7) (shift k #f)]
              [(equal? (take-right program matching) digits)
               (define ans (reset (loop (add1 matching) (+ m (* 8 result)))))
               (if (not ans) (inner (add1 m)) ans)]
              [(inner (add1 m))]))))
