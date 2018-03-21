(define (filter pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (cons (car items)
               (filter pred (cdr items))))
        (else (filter pred (cdr items)))))

(define (quicksort lst)
  (if (null? lst)
      lst
      (append
       (quicksort (filter (lambda (x) (<= x (car lst))) (cdr lst)))
       (list (car lst))
       (quicksort (filter (lambda (x) (> x (car lst))) (cdr lst))))))

(define (last-pair lst)
 (if (null? (cdr lst))
     lst
     (last-pair (cdr lst))))

(define (reverse2 lst)
  (define (reverse-inner new old)
    (if (null? old)
        new
        (reverse-inner (cons (car old) new) (cdr old))))
  (reverse-inner '() lst))

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(define (for-each2 proc lst)
  (if (null? lst)
      'ok
      (begin
        (proc (car lst))
        (for-each2 proc (cdr lst)))))

(define (equal2? a b)
  (or (eq? a b)
      (and (pair? a) (pair? b)
           (equal2? (car a) (car b))
           (equal2? (cdr a) (cdr b)))))
          