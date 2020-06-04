(define (valid-month-31? month)
  (member? month '(1 3 5 7 8 10 12)))
(define (valid-month-30? month)
  (member? month '(4 6 9 11)))
(define (valid-month-2? month)
  (member? month '(2)))

(define (valid-day-31? day)
  (member? day '(1 2 3 4 5 6 7 8 9 10
                   11 12 13 14 15 16 17 18 19 20
                   21 22 23 24 25 26 27 28 29 30 31)))
(define (valid-day-30? day)
  (member? day '(1 2 3 4 5 6 7 8 9 10
                   11 12 13 14 15 16 17 18 19 20
                   21 22 23 24 25 26 27 28 29 30)))
(define (valid-day-29? day)
  (member? day '(1 2 3 4 5 6 7 8 9 10
                   11 12 13 14 15 16 17 18 19 20
                   21 22 23 24 25 26 27 28 29)))
(define (valid-day-28? day)
  (member? day '(1 2 3 4 5 6 7 8 9 10
                   11 12 13 14 15 16 17 18 19 20
                   21 22 23 24 25 26 27 28)))

(define (leap-year-100? year) (equal? (modulo year 100) 0))
(define (leap-year-400? year) (equal? (modulo year 400) 0))
(define (leap-year-4? year) (equal? (modulo year 4) 0))
(define (leap-year? year)
  (if (leap-year-100? year)
      (leap-year-400? year)
      (leap-year-4? year)))

(define (valid-date-31? month day year)
  (and (valid-month-31? month) (valid-day-31? day)))
(define (valid-date-30? month day year)
  (and (valid-month-30? month) (valid-day-30? day)))
(define (valid-date-29? month day year)
  (and (leap-year? year) (valid-month-2? month) (valid-day-29? day)))
(define (valid-date-28? month day year)
  (and (valid-month-2? month) (valid-day-28? day)))

(define (valid-date? month day year)
  (or (valid-date-31? month day year) (valid-date-30? month day year)
      (valid-date-29? month day year) (valid-date-28? month day year)))
