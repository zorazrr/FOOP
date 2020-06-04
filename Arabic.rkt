(define (roman-value letter)
  (cond ((equal? letter 'i) 1)
        ((equal? letter 'v) 5)
        ((equal? letter 'x) 10)
        ((equal? letter 'l) 50)
        ((equal? letter 'c) 100)
        ((equal? letter 'd) 500)
        ((equal? letter 'm) 1000)
        (else 'huh?)))

(define (second letter) (first (bf letter)))

(define (arabic letter)
  (cond ((= 1 (count letter)) (roman-value letter))
        ((= 0 (count letter)) 0)
        ((< (roman-value (first letter)) (roman-value (second letter)))
         (+ (- (roman-value (second letter)) (roman-value (first letter))) (arabic (bf (bf letter)))))
        (else (+ (roman-value (first letter)) (arabic (bf letter))))))
 
(trace arabic)
 