(define ranks '(a k q j 10 9 8 7 6 5 4 3 2))
(define suits '(s h d c))

(define (numeric-rank rank)
   (cond ((equal? rank 'a) 14)
         ((equal? rank 'k) 13)
         ((equal? rank 'q) 12)
         ((equal? rank 'j) 11)
         (else rank)))

(define (convert-suit suit) ;convert simplified suit name into original name
  (cond ((equal? suit 'h) 'heart)
        ((equal? suit 's) 'spade)
        ((equal? suit 'd) 'diamond)
        ((equal? suit 'c) 'clover)))

(define (make-card suit rank) (word suit rank))
(define (rank card) (bf card)) ;rank of a card
(define (suit card) (first card)) ;suit of a card

(define (sort hand) ;sort hand in order 
   ((repeated sort-once (- (count hand) 1)) hand))

(define (sort-once hand)
   (cond ((empty? hand) hand)
         ((= (count hand) 1) hand)
         ((> (numeric-rank (bf (first hand)))
             (numeric-rank (bf (first (bf hand)))))
                 (se (first hand) (sort-once (bf hand))))
         (else (se (first (bf hand))
                   (sort-once (se (first hand) (bf (bf hand))))))))

(define (get-suits hand) ;take out all the suits in a hand
  (cond ((empty? hand) hand)
        (else (se (suit (first hand)) (get-suits (bf hand))))))

(define (get-ranks hand) ;take out all the ranks in a hand
  (cond ((empty? hand) hand)
        (else (se (rank (first hand)) (get-ranks (bf hand))))))

(define (get-differences hand) ;get the differences between all ranks
  (cond ((equal? (count hand) 1) '())
        (else (se (- (numeric-rank (rank (first hand))) (numeric-rank (rank (first (bf hand))))) (get-differences (bf hand))))))

(define (every-equal? hand) ;test if the ranks are the same
  (cond ((equal? (count hand) 1) #t)
        ((not (equal? (first hand) (first (bf hand)))) #f)
        (else (every-equal? (bf hand)))))

(define (same-2-ranks-helper base num)
  (cond ((empty? num) base)
        ((equal? (first num) 0) (same-2-ranks-helper (+ 1 base) (bf num)))
        (else (same-2-ranks-helper (+ 0 base) (bf num)))))
(define (same-4-ranks? num) ;test if 4 of the ranks are the same
  (cond ((or (not (equal? (item 2 num) 0)) (not (equal? (item 3 num) 0))) #f)
    ((equal? (same-2-ranks-helper 0 num) 3) #t)
        (else #f)))
(define (same-3-ranks? num) ;test if 3 out of the ranks are the same 
  (cond ((equal? (same-2-ranks-helper 0 num) 2) #t)
        (else #f)))
(define (same-2-ranks? num)
  (cond ((equal? (same-2-ranks-helper 0 num) 1) #t)
        (else #f)))
(define (two-same-2-ranks? num)
  (cond ((equal? (same-2-ranks-helper 0 num) 2) #t)
        (else #f)))
(define (same-4-ranks-2? num)
  (cond ((equal? (same-2-ranks-helper 0 num) 3) #t)
        (else #f)))

(define (final-suit hand) ;gets the final suit for flushes
  ((convert-suit (suit (first hand)))))

(define (counts-helper num k list)
  (cond ((empty? list) num)
        ((equal? k (first list)) (counts-helper (+ 1 num) k (bf list)))
        (else (counts-helper (+ 0 num) k (bf list)))))
(define (counts k list)
  (counts-helper 0 k list))

(define (final-rank hand)
  (rank (item 3 hand)))

(define (get-highest hand)
  (rank (last hand)))

(define (final-big-fullhouse hand)
  (cond ((equal? (counts (first hand) hand) 3) (first hand))
        (else (last hand))))
(define (final-small-fullhouse hand)
  (cond ((equal? (counts (first hand) hand) 2) (first hand))
        (else (last hand))))

(define (final-pair hand)
  (cond ((empty? hand) hand)
        ((equal? (counts (first hand) hand) 2) (first hand))
        (else (final-pair (bf hand)))))

(define (2-consequent-0s num)
  (cond ((equal? (count num) 1) #f)
        ((equal? (- (first num) (first (bf num))) 0) #t)
        (else (2-consequent-0s (bf num)))))
                               

(define (straight-flush? hand) ;straight flush 
  (cond ((not (equal? (get-differences hand) '(1 1 1 1))) #f)
        ((not (every-equal? (get-suits hand))) #f)
        (else #t)))

(define (royal-flush? hand) ;royal flush
  (let ((sortedhand (sort hand)))
    (cond ((not (straight-flush? sortedhand)) #f)
          ((not (equal? (get-ranks sortedhand) '(a k q j 10))) #f)
          (else #t))))

(define (four-of-a-kind? hand) ;four of a kind
  (cond ((not (same-4-ranks? (get-differences hand))) #f)
        (else #t)))

(define (flush? hand) ;flush 
  (cond ((not (every-equal? (get-suits hand))) #f)
        (else #t)))

(define (three-of-a-kind? hand) ;three of a kind
  (cond ((not (same-3-ranks? (get-differences hand))) #f)
        ((equal? (last (get-differences hand)) 0) #f)
        (else #t)))

(define (straight? hand) ;straight
  (cond ((every-equal? (get-suits hand)) #f)
        ((not (equal? (get-differences hand) '(1 1 1 1))) #f)
        (else #t)))

(define (full-house? hand) ;fullhouse
  (cond ((not (same-4-ranks-2? (get-differences hand))) #f)
        ((and (not (equal? (first (get-differences hand))  0)) (not equal? (last (get-differences hand))  0))  #f)
        (else #t)))

(define (pair? hand) ;hand
  (cond ((not (same-2-ranks? (get-differences hand))) #f)
        (else #t)))

(define (two-pair? hand) ;two pairs
  (cond ((not (two-same-2-ranks? (get-differences hand))) #f)
        ((2-consequent-0s (get-differences hand)) #f)
        (else #t)))


(define (poker-value hand) ;final code 
  (let ((sortedhand (sort hand)))
    (cond ((royal-flush? sortedhand) (se '(ROYAL FLUSH-) (convert-suit (suit (first sortedhand)))))
          ((straight-flush? sortedhand) (se '(STRAIGHT FLUSH-) (convert-suit (suit (first sortedhand))) '(with highest) (get-highest hand)))
          ((four-of-a-kind? sortedhand) (se '(FOUR OF A KIND-) (final-rank sortedhand)))
          ((full-house? sortedhand) (se '(FULL HOUSE-) (final-big-fullhouse (get-ranks hand)) 'over (final-small-fullhouse (get-ranks hand))))
          ((flush? sortedhand) (se '(FLUSH-) (convert-suit (suit (first sortedhand)))))
          ((straight? sortedhand) (se (get-highest hand) '(-high STRAIGHT)))
          ((three-of-a-kind? sortedhand) (se '(THREE OF A KIND-) (final-rank sortedhand)))
          ((two-pair? sortedhand) (se '(TWO PAIR of) (final-pair (get-ranks sortedhand))))
          ((pair? sortedhand) (se '(PAIR of) (final-pair (get-ranks sortedhand))))
          (else 'NOTHING))))
