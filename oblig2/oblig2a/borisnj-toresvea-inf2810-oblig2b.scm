;;Gruppe
;; Brukernavn: toresvea og borisnj
;; Navn: Tore Sveaas og Henrik Johnsen

(load "huffman.scm")

;; Oppgave 1

;; a
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

"Utskrift 1a"
(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))



;; b
(define foo 42)
"Utskrift Oppgave 1b"
"Bruker 'let' første del"
(let ((foo 5)
      (x foo))
  (if (= x foo)
      'same
      'different))
"Bruker 'lambda' første del"
((lambda (foo x)
   (if (= x foo)
      'same
      'different)) 5 foo)

"Bruker 'let' andre del"
(let ((bar foo)
      (baz 'towel))
  (let ((bar (list bar baz))
        (foo baz))
    (list foo bar)))

"Bruker 'lambda' andre del"
((lambda (bar baz)
  ((lambda (bar foo)
     (list foo bar))
   (list bar baz) baz))
 foo 'towel)



;; c
"Utskrift 1c"
(define (infix-eval items)
  ((cadr items) (car items) (caddr items)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo)
(infix-eval baz)
(infix-eval bar)



;; d
(define bah '(84 / 2))
;; (infix-eval bah) -> application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: /
;;    arguments...:
;; Siden vi setter quote foran blir ingenting evaluert, og '\' blir ansett
;; som et tegn, og ikke en prosedyre.
;;


;;Oppgave2

;;a
(define (member? eq_pred element liste)
  (if (null? liste)
      #false
      (if (eq_pred element (car liste))
          #t
          (member? eq_pred element (cdr liste)))))


"Test 2a"
(member? eq? 'zoo '(bar foo zap))
(member? eq? 'foo '(bar foo zap))
(member? = 1 '(3 2 1 0))
(member? eq? '(1 bar)
'((0 foo) (1 bar) (2 baz)))
(member? equal? '(1 bar)
'((0 foo) (1 bar) (2 baz)))


;; b 
(define (decode-tr bits tree)       
  (define (decode-1 bits current-branch current-list)
    (if (null? bits)
        (reduce-reverse cons '() current-list)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) current-list))
              (decode-1 (cdr bits) next-branch current-list)))))
  (decode-1 bits tree '()))

(define (reduce-reverse proc init items)
  (define (reduce-rev-inner new-list list)
    (if (null? list)
        new-list
        (reduce-rev-inner (proc (car list) new-list) (cdr list))))
  (reduce-rev-inner init items))

;; c 
"Utskrift 2c"
(decode-tr sample-code sample-tree)
"Utskriften blir reversert. Kunne reversert som i forrige oblig"

;; d
(define (encode seq tree)
  (define (encode-inner seq bits sub-tree)
    (if (null? seq)
        (reduce-reverse cons '() bits)
        (if (leaf? sub-tree)
            (encode-inner (cdr seq) bits tree)
            (cond ((member? eq? (car seq) (symbols (left-branch sub-tree)))
                   (encode-inner seq (cons 0 bits) (left-branch sub-tree)))
                  ((member? eq? (car seq) (symbols (right-branch sub-tree)))
                   (encode-inner seq (cons 1 bits) (right-branch sub-tree)))))))
  (encode-inner seq '() tree))

(define (teste a)
  (cond ((eq? "a" a) "Jepp")
        (else "Nope")))
               


  
;Test kode
(decode (encode '(ninjas fight ninjas by night) sample-tree) sample-tree)

; 
