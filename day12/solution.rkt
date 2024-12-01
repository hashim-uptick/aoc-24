#lang typed/racket

(define-type Grid (Listof (Listof Char)))
(: grid Grid)
(define grid (cast (map string->list (with-input-from-file "input.txt" port->lines)) Grid))

(: in-bounds? (-> Grid Integer Integer Boolean))
(define (in-bounds? grid x y)
  (not (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (list-ref grid 0))))))

(: value-at (-> Grid Number (U False Char)))
(define (value-at grid at)
  (let ([x : Integer (cast (real-part at) Integer)] [y : Integer (cast (imag-part at) Integer)])
    (and (in-bounds? grid x y)
         (list-ref (list-ref grid y) x))))

(: neighbouring (-> Number (Listof Number)))
(define (neighbouring at)
  (list (+ at -i) (+ at -1) (+ at 1) (+ at +i)))

(: valid-neighbours (-> Grid Number (Listof Number)))
(define (valid-neighbours grid at)
  (filter (λ([pt : Number])
            (in-bounds? grid (cast (real-part pt) Integer) (cast (imag-part pt) Integer)))
          (neighbouring at)))

(: find-neighbours-matching (-> Grid Number Char (Listof Number)))
(define (find-neighbours-matching grid at val)
  (filter (λ([pt : Number]) (equal? val (value-at grid pt))) (valid-neighbours grid at)))

(: find-regions (-> Grid (HashTable Integer (Setof Number))))
(define (find-regions grid)
  (: find-region (-> Grid Char (Listof Number) (Setof Number) (Setof Number)))
  (define (find-region grid ch test-points visited)
    (define-values (match no-match) (partition (λ([loc : Number]) (equal? (value-at grid loc) ch)) test-points))
    (define visited^ (set-union visited (list->set test-points)))
    (if (null? match)
        (set)
        (set-union (list->set match)
                   (find-region grid
                                ch
                                (filter (λ([loc : Number]) (not (set-member? visited^ loc)))
                                        (remove-duplicates (append-map
                                                            (λ([loc : Number])
                                                              (valid-neighbours grid loc))
                                                            match)))
                                visited^))))
  (define all-points (list->set
                      (append-map (λ([y : Real]) (map (λ([x : Real]) (make-rectangular x y))
                                                      (range (length (first grid)))))
                                  (range (length grid)))))
  (for/fold
   ([regions : (HashTable Integer (Setof Number)) (hash)]
    [points : (Setof Number) all-points]
    #:result regions)
   ([n (in-naturals)]
    #:break (set-empty? points))
    (define next-loc (set-first points))
    (define next-val (cast (value-at grid next-loc) Char))
    (define region-set (find-region grid next-val (list next-loc) (set)))
    (values (hash-set regions n region-set)
            (set-subtract points region-set))))

(: perimeter (-> Grid (Setof Number) Number))
(define (perimeter grid region)
  (for/sum ([loc (in-set region)])
    (- 4 (count (λ([neighbour-loc : Number]) (equal? (value-at grid loc) (value-at grid neighbour-loc)))
                (find-neighbours-matching grid loc (cast (value-at grid loc) Char))))))

(: area (-> (Setof Number) Number))
(define (area region) (set-count region))

(define regions (find-regions grid))

;; sanity check: the following should equal the grid area.  ;; 19600
(apply + (map (λ([x : (Setof Number)]) (set-count x)) (hash-values regions)))

;; part 1
(for/sum : Number ([region (hash-values regions)])  ;; 1359028
  (* (perimeter grid region) (area region)))


;; part 2


;; For one thing, Racket's built-in groupby considers the whole list, and for another,
;; I specifically need to compare two thing at a time
(: window-by (All (A) (-> (Listof A) (-> A A Boolean) (Listof (Listof A)))))
(define (window-by lst cmp)
  (: comparer (-> A (Listof (Listof A)) (Listof A) (Listof (Listof A))))
  (define (comparer val acc lst)
    (if (null? lst)
        (reverse (map (λ([x : (Listof A)]) (reverse x)) acc))
        (if (cmp val (first lst))
            (comparer (first lst) (cons (cons (first lst) (first acc)) (rest acc)) (rest lst))
            (comparer (first lst) (cons (list (first lst)) acc) (rest lst)))))
  (if (null? lst)
      '()
      (comparer (first lst) (list (list (first lst))) (rest lst))))

(define (consecutive-x? [a : Number] [b : Number]) : Boolean
  (= 1 (- (real-part b) (real-part a))))

(define (consecutive-y? [a : Number] [b : Number]) : Boolean
  (= 1 (- (imag-part b) (imag-part a))))

(: sides (-> Grid (Setof Number) Number))
(define (sides grid region)
  (define val (cast (value-at grid (set-first region)) Char))
  (define region-list (set->list region))
  (define horizontal-lines (map (λ([locs : (Listof Number)])
                                  (sort locs (λ([n : Number] [m : Number])
                                               (< (real-part n) (real-part m)))))
                                (group-by (λ([x : Number]) (imag-part x)) region-list)))
  (define vertical-lines (map (λ([locs : (Listof Number)])
                                  (sort locs (λ([n : Number] [m : Number])
                                               (< (imag-part n) (imag-part m)))))
                                (group-by (λ([x : Number]) (real-part x)) region-list)))
  (define top-sum
    (for/sum : Number ([line horizontal-lines])
      (if (= 0 (imag-part (first line)))
          (length (window-by line consecutive-x?))
          (length (window-by (filter-not (λ([loc : Number]) (equal? (value-at grid (+ loc -i)) val))
                                         line)
                             consecutive-x?)))))
  (define bottom-sum
    (for/sum : Number ([line horizontal-lines])
      (if (= (sub1 (length grid)) (imag-part (first line)))
          (length (window-by line consecutive-x?))
          (length (window-by (filter-not (λ([loc : Number]) (equal? (value-at grid (+ loc +i)) val))
                                         line)
                             consecutive-x?)))))
  (define left-sum
    (for/sum : Number ([line vertical-lines])
      (if (= 0 (real-part (first line)))
          (length (window-by line consecutive-y?))
          (length (window-by (filter-not (λ([loc : Number]) (equal? (value-at grid (+ loc -1)) val))
                                         line)
                             consecutive-y?)))))
  (define right-sum
    (for/sum : Number ([line vertical-lines])
      (if (= (sub1 (length (first grid))) (real-part (first line)))
          (length (window-by line consecutive-y?))
          (length (window-by (filter-not (λ([loc : Number]) (equal? (value-at grid (+ loc 1)) val))
                                         line)
                             consecutive-y?)))))
  (+ top-sum bottom-sum left-sum right-sum))

(for/sum : Number ([region (hash-values regions)])  ;; 839780
  (* (sides grid region) (area region)))
