;Oppgave 1

;(a)
(* (+ 2 2) 5);Her er output 20 (4*5)
;(b) 
;(* (+ 2 2) (5)) Denne vil ikke fungere fordi (5) ikke er en gyldig prosedyre
;(c) 
;(* (2 + 2) 5) Denne vil ikke fungere fordi (2 + 2) ikke er en gyldig prosedyre.
;Riktig syntaks ville vært (+ 2 2).
;(d) 
(define bar (/ 4 2)) bar;Her er output 2 (4/2), prosedyren bar blir kalt.
;(e)
(- bar 2); Her blir output 0. bar (som er 2) minus 2 er 0.
;(f)
(/ (* bar 3 4 1) bar);Her blir output 12. 2*3*4*1/2 = 12.

;Oppgave 2

;(a)
 (or (= 1 2)
       "piff!"
       "paff!"
       (zero? (1 - 1))); Her vil output være "piff!" fordi det er første argument
;som blir evaluert til "true". Her er det syntaks feil (1 - 1), men fordi 
;en returverdi allerede er gitt blir ikke resten av uttrykket evaluert.

   (and (= 1 2)
        "piff!"
        "paff!"
        (zero? (1 - 1))); Her vil output være #f(false) fordi ikke alle argumenter
;er "true" og #f blir da returnert. Samme syntaksfeil som ovenfor.

   (if (positive? 42)
       "poff!"
       (i-am-undefined)); Her vil output være "poff!". Her kaller den på en 
;prosedyre som ikke er definert, men av samme grunner som ovenfor vil ikke 
;dette bli evaluert fordi returverdien allerede er gitt.

;(b)
;Med IF:
(define (sign x)
    (if(positive? x)
       1
    (if(negative? x)
       -1
    (if(zero? x)
       0))))

;Med COND:
(define (sign2 x)
    (cond((positive? x) '1)
         ((negative? x) '-1)
         (else '0)))

;(c)
;Med AND og OR
(define (sign3 x)
    (or (and (positive? x)'1)
        (and (negative? x)'-1)
        (and (zero? x)'0)))

;Oppgave 3

;(a)

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

;(b)

(define (plus x y)
  (if (zero? x)
      y
      (if (zero? y)
          x
          (plus (add1 x) (sub1 y)))))

;(c)

;Den rekursive prosedyren ovenfor begynner med å sjekke om et av parametrene 
;er null. Dersom et av dem er null vil svaret være det andre parameteret.
;Dersom ingen av dem er null vil den kalle seg selv, hvor de nye parametrene
;x og y er x+1 og y-1. Dermed vil det legges til like mange i "x" som det trekkes
;fra i "y". Prosessen er såkalt halerekursiv som betyr at til slutt når 
;"y" blir lik null vil den returnere x som da er svaret.
;Derfor kan jeg si at dette er en iterativ prosess. Når vi er kommet til 
;"basis tilfellet" (0) har den også svaret med en gang, det er ingen regning
;som gjenstår. Prosessen regner ut svaret underveis i de rekursive kallene.

;Rekursiv prosess:

(define (plus2 x y)
  (if (zero? y)
      x
      (if (zero? x)
          y
          (add1 (plus2 x (sub1 y))))))

;(d)

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))
;Jeg har flyttet definisjonen av power-iter innenfor definisjonen til 
;power-close-to. Dette skaper en såkalt blokkstruktur. 
; Jeg har også forenklet definisjonen til power-iter. Dette lar seg gjøre
;fordi "b" og "n" allerede er mottatt i power-close-to. Dette er konstanter
;og de trenger ikke bli sendt med på nytt.