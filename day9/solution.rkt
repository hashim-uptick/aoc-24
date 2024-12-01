#lang racket

;; Warning: these solutions are dumb and slow.

(define inp (string-trim (with-input-from-file "input.txt" port->string)))

;; test-inp ;(define inp "2333133121414131402")
(define disk-map (map (compose string->number string) (string->list inp)))

(struct File (id size) #:transparent)

(define (free-file? f) (equal? (File-id f) 'free))

(define (take-upto-size f size)
  (if (>= size (File-size f))
      f
      (File (File-id f) size)))

(define (right-most-file+idx map)
  (for/last ([(f i) (in-indexed map)]
             #:when (not (free-file? f))) (cons f i)))

(define file-map
  (for/list ([(digit i) (in-indexed disk-map)])
    (File (if (even? i) (/ i 2) 'free) digit)))

(define (merge-frees map)
  (reverse
   (for/fold ([acc '()])
             ([f map])
     (if (free-file? f)
         (if (free-file? (first acc))
             (list-update acc 0 (λ(f^) (File 'free (+ (File-size f) (File-size f^)))))
             (cons f acc))
         (cons f acc)))))

(define after-run
  (let loop ([remaining file-map])
    (if (null? remaining)
        '()
        (let ([next (first remaining)])
          (if (not (free-file? next))
              (cons (first remaining) (loop (rest remaining)))
              (let ([free-size (File-size next)]
                    [maybe-file+idx (right-most-file+idx (rest remaining))])
                (if (not maybe-file+idx)
                    '()
                    (let ()
                      (match-define (cons right-file idx) maybe-file+idx)
                      (define piece-of-file (take-upto-size right-file free-size))
                      (define remaining^^
                        (append (take (rest remaining) idx)
                                (if (> (File-size right-file) (File-size piece-of-file))
                                    (cons (File (File-id right-file) (- (File-size right-file) free-size))
                                          (cons (File 'free free-size)
                                                (drop (rest remaining) (add1 idx))))
                                    (cons (File 'free (File-size right-file))
                                          (drop (rest remaining) (add1 idx))))))
                      (define remaining^
                        (if (= (File-size piece-of-file) free-size)
                            (cons piece-of-file remaining^^)
                            (cons piece-of-file
                                  (cons (File 'free (- free-size (File-size piece-of-file)))
                                        remaining^^))))
                      (loop (merge-frees remaining^))))))))))

(define checksum
  (for/fold ([block-num 0]
             [check-sum 0]
             #:result check-sum)
            ([f after-run])
    (values (+ block-num (File-size f))
            (if (free-file? f)
                check-sum
                (+ check-sum
                   (for/sum ([block-num^ (range block-num (+ block-num (File-size f)))])
                     (* block-num^ (File-id f))))))))

; checksum  ;; 6353658451014

(define after-run-2
  (for/fold ([state file-map])
            ([f (reverse file-map)]
             #:when (not (free-file? f)))
    (define i-from-left (index-where state (λ(lf) (equal? (File-id lf) (File-id f)))))
    (define free-candidate
      (for/first ([(free-f i) (in-indexed (take state i-from-left))]
                  #:when (and (free-file? free-f) (>= (File-size free-f) (File-size f))))
        (cons free-f i)))
    (match free-candidate
      [#f state]
      [(cons candidate-f candidate-i)
       (if (= (File-size candidate-f) (File-size f))
           (merge-frees (list-update (list-update state candidate-i (λ(_) f))
                                     i-from-left
                                     (λ(_) candidate-f)))
           (merge-frees (append (take state candidate-i)
                                (list f
                                      (File 'free (- (File-size candidate-f) (File-size f))))
                                (take (drop state (add1 candidate-i)) (- i-from-left candidate-i 1))
                                (list (File 'free (File-size f)))
                                (drop state (add1 i-from-left)))))])))

(define checksum-2
  (for/fold ([block-num 0]
             [check-sum 0]
             #:result check-sum)
            ([f after-run-2])
    (values (+ block-num (File-size f))
            (if (free-file? f)
                check-sum
                (+ check-sum
                   (for/sum ([block-num^ (range block-num (+ block-num (File-size f)))])
                     (* block-num^ (File-id f))))))))

checksum-2
