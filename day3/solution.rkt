#lang racket

(define inp (with-input-from-file "input.txt" port->string))

(define matches (regexp-match* #px"mul\\(\\d{1,3},\\d{1,3}\\)" inp))

(define (execute-mul s)
  (match s
    [(pregexp #px"mul\\((\\d{1,3}),(\\d{1,3})\\)" (list _ l r))
     (* (string->number l) (string->number r))]))

(apply + (map execute-mul matches))

(define (run-program s [acc 0] [on? #t])
  (if on?
      (match s
        [(pregexp #px"(don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\))(.*)" (list _ inst remaining))
         (if (string-prefix? inst "mul")
             (run-program remaining (+ acc (execute-mul inst)) #t)
             (run-program remaining acc #f))]
        [_ acc])
      (match s
        [(pregexp #px"do\\(\\)(.*)" (list _ remaining))
         (run-program remaining acc #t)]
        [_ acc])))

(run-program inp)
