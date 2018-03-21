;; Oblig 1b Tore Sveaass toresvea inf2810
;; Resultatene for hver oppgave printes ut. Mulig det gjør koden litt rotete å lese.
;;
;; Oppgave 1
;; a) [ * | * ]->11
;;      |
;;     47
;;
;; b) [ * | / ]
;;      |
;;     47
;;
;; c) [ * | * ]->[ * | / ]
;;      |          |
;;     47         11
;;
;; d) [ * | * ]->[ * | / ]
;;      |          |
;;     47        [ * | * ] -> [ * | / ]
;;                 |            |
;;                11            12
;;
;; e)
;;                        
;;    [ * | * ]----------------------------
;;      |                                 |                           
;;    [ * | * ]->[ * | * ]->[ * | / ]   [ * | * ]->[ * | * ]->[ * | / ]
;;      |          |          |           |          |          | 
;;      1          2          3           1          2          3
;;
;; f)
;;
;; (car (cdr '(0 42 #t bar)))
;;
;; g)
;; (car (cdr (car '((0 42) (#t bar)))))
;;
;; h)
;;  (car (car (cdr '((0) (42 #t) (bar)))))
;;
;; i)
;; (list (list 0 42) (list #t 'bar))
;; (cons (cons 0 (cons 42 '())) (cons (cons #t (cons 'bar '())) '()))
;;
"Oppgave 2"
"a)"
(define (length2 list)
  (define (length2-count items count)
    (if (null? items)
         count
        (length2-count (cdr items) (+ 1 count))))
  (length2-count list 0))
"(length2 '(1 2 3 4)"
(length2 '(1 2 3 4))
         
"b)"
;; Lager prosedyren halerekursiv som gir opphav til en iterativ prosess

(define (reduce-reverse proc init items)
  (define (reduce-rev-inner new-list list)
    (if (null? list)
        new-list
        (reduce-rev-inner (proc (car list) new-list) (cdr list))))
  (reduce-rev-inner init items))
"(reduce-reverse cons '() '(1 2 3 4))"
(reduce-reverse cons '() '(1 2 3 4))
"c)"
;;
(define (all? pred items)
  (if (null? items)
      #t
      (if (pred (car items))
          (all? pred (cdr items))
          #f)))
      
"Med lambda"
"(all? (lambda (x) (<= x 10)) '(1 2 3 4 10))"
(all? (lambda (x) (<= x 10)) '(1 2 3 4 10))
"(all? (lambda (x) (<= x 10)) '(1 2 3 4 50))"
 (all? (lambda (x) (<= x 10)) '(1 2 3 4 50))

"d)"
(define (nth index list)
  (define (nth-counter count iter-list)    
    (if (equal? index count)
        (car iter-list)
        (nth-counter (+ 1 count) (cdr iter-list))))
  (nth-counter 0 list))

"(nth 2 '(47 11 12 13))"    
 (nth 2 '(47 11 12 13))
;;
"e)"
(define (where value list)
  (define (where-iter count iter-list)
    (if (null? iter-list)
        #f
        (if (equal? value (car iter-list))
            count
            (where-iter (+ 1 count) (cdr iter-list)))))
  (where-iter 0 list))

"(where 3 '(1 2 3 3 4 5 3))"
(where 3 '(1 2 3 3 4 5 3))
"(where 0 '(1 2 3 3 4 5 3))"
(where 0 '(1 2 3 3 4 5 3))

"f)"
(define (map2 proc list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (cons (proc (car list1) (car list2)) (map2 proc (cdr list1) (cdr list2)))))

"(map2 + '(1 2 3 4) '(3 4 5))"
(map2 + '(1 2 3 4) '(3 4 5))

"g)"
"(map2 (lambda (x y) (/ (+ x y ) 2)) '(1 2 3 4) '(3 4 5))"
(map2 (lambda (x y) (/ (+ x y ) 2)) '(1 2 3 4) '(3 4 5))

"h)"
(define (both? pred)
  (lambda (x y) (and (pred x) (pred y))))

"(map2 (both? even?) '(1 2 3) '(3 4 5))"
(map2 (both? even?) '(1 2 3) '(3 4 5))
"((both? even?) 2 4)"
((both? even?) 2 4)
"((both? even?) 2 5)"
((both? even?) 2 5)

"i)"
(define (self proc)
  (lambda (x) (proc x x)))

"((self +) 5)"
((self +) 5)
"((self *) 3)"
((self *) 3)
"(self +)"
(self +)
"((self list) 'hello')"
((self list) "hello")