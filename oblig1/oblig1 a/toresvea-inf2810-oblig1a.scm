;; Oppgave 1
;; a)
;; (* (+ 4 2) 5) blir evaluert til 30.
;; Først blir (+ 4 2) evaluert til 6 slik at vi har
;; (* 6 5), som igjen blir evaluert til 30
;;
;; b)
;; (* (+ 4 2) (5)) vil gi en feilmelding da listen (5) mangler
;; en prosedyre
;;
;; c)
;; (* (4 + 2) 5) blir ikke evaluert da operatoren, +, ikke
;; er det første uttrykket i listen (4 + 2)
;;
;; d)(define bar (/ 42 2))
;;   bar
;; I dette tilfellet blir uttrykket evaluert til 21,
;; og variabelen bar blir definert til 21. I neste linje
;; kalles bar og 21 skrives ut
;;
;; e)
;; (- bar 11) evalueres til 21 - 11 - > 10
;;
;; f)
;; (/ (* bar 3 4 1) bar). Her er bar fortsatt lik 21 og
;; det sammensatte uttrykket blir evaluert til 21*3*4*1. Deretter
;; ser vi at det første uttrykket er / slik av vi får (21*12) / 21
;; som gir oss 12
;;
;; Oppgave 2
;; a)
;;
;; (or (= 1 2)
;;     "piff!"
;;     "paff!"
;;     (zero? (1 - 1)))
;; Riktig syntaks er (- 1 1)
;; Så lenge et av uttrykkene evalueres til sann vil
;; hele uttrykket evalueres til sann. I tillegg vil det
;; første uttrykket som evalueres til sann bli skrevet ut.
;; I dette tilfellet "piff!"
;;
;;(and (= 1 2)
;;     "piff!"
;;     "paff!"
;;     (zero? (1 - 1)))
;; Nå må alle uttrykkene evalueres til sann for at hele
;; uttrykket skal evalueres til sann. Siden 1 != 2 stopper
;; det opp før vi kommer til slutten med dårlig syntaks og
;; #f blir skrevet ut
;;
;;(if (positive? 42)
;;    "poff!"
;;    (i-am-undefined))
;; Her sjekkes det om 42 er positivt. Siden det er det
;; blir "poff!" skrevet ut, og programmet går ut
;; av evalueringen. Hvis vi hadde satt 42 til -42 ville
;; vi fått en klage på at (i-am-undefined) ikke er definert
;;
;; Or, and og if er special form siden de ikke evaluerer alle argumentene
;; hvis ett av argumentene gjør at det ikke er behov. Vi ser f.eks. at over
;; at (i-am -undefined) ikke blir evaluert siden 42 er et positivt tall.
;;
;; b)
(define (sign-if arg)
  (if (= arg 0)
      arg
      (if (> arg 0)
          1
          -1)))

(define (sign-cond arg)
  (cond ((> arg 0) 1)
        ((= arg 0) 0)
        ((< arg 0) -1)))
;;
;; c)
(define (sign-pred arg)
  (or (and (> arg 0)
           1)
      (and (= arg 0)
           0)
      (- 1)))
;;
;; Oppgave 3
;; a)
(define (add1 arg)
  (+ arg 1))

(define (sub1 arg)
  (- arg 1))
;; b)
(define (plus arg1 arg2)
  (if(zero? arg2)
     arg1
     (plus (add1 arg1) (sub1 arg2))))
;;
;; c)
;; Rekursiv variant:
(define (plus-rec arg1 arg2)
  (if (= 0 arg2)
      arg1
      (add1 (plus-rec arg1 (sub1 arg2)))))
;; Jeg har brukt en iterativ prosess i oppgave b). Over har jeg gjort det rekursivt.
;; Den store forskjellen er at i iterative prosesser blir mellomresultatet lagret
;; og disse verdiene blir kalt rekursivt. I dette tilfellet er ikke det første kallet
;; avhengig av resultatene fra det siste kallet i køen. Av denne grunn er O-space konstant.
;; For oppgave b) vil 5+2 evalueres slik:
;; (plus 5 2)
;; (plus 6 1)
;; (plus 7 0)
;; 7
;; 
;; For den rekursive prosessen med samme tilfelle får vi
;; (plus-rec 5 2)
;; (add1 plus-rec (5 1))
;; (add1 (add1 (plus-rec 5 0)))
;; (add1 (add1 5))
;; (add1 6)
;; 7
;;
;; d)
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))
;;
;; Siden hjelpeprosedyren har tilgang til b og n kan den forenkles ved å fjerne disse argumentene.
;;
;; e)
;;
;; Inspirert av oppgave d) kan det tenkes at å forenkle ved å fjerne n / count fra hjelpefunksjonen vil gå siden funksjonen
;; har tilgang til samme argumenter som hovedfunksjonen. Problemet er at count endrer seg fra n under beregningene. Derfor må
;; fib-iter beholde n / count som argument. Forenklingen gjennom blokkstruktur er vist under.
(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        b
        (fib-iter (+ a b) a (- n 1))))
  (fib-iter 1 0 n))
;;
;;