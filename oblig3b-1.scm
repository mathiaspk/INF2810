;;(require r5rs)
;;(print-as-expression #f)
;;(print-mpair-curly-braces #f)
;;(load "evaluator.scm")

;; Stian Kongsvik (stiako) & Mathias Källström (mathiapk)
;; Oblig 3B

;; Oppg. 1
;; (a)
;; (foo 2 square) returnerer 0, dette fordi i foo vil argumentet kalt cond være
;; 2, og da vil predikatet (= cond 2) bli #t og returverdien blir da 0.
;; 
;; (foo 4 square) blir 16, ettersom her vil (= cond 2) bli #f, og else vil slå ut og
;; kalle prosedyren square som argumentet kalt else er i dette tilfellet.
;;
;; (cond ((= cond 2) 0)
;;         (else (else 4))) vil returnere 2, ettersom her vil cond være 3, mens prosedyren
;; else (som deler argumentet det får på 2) blir kalt.
;;
;; Det er sjekker for om et uttrykk er et "innebygd" scheme uttrykk i "evaluator.scm" (prekoden).
;; Denne sjekken finnes for alle uttrykk som er definert i språket, og benytter seg av "tagged-list?"
;; som sjekker om et uttrykk har den nødvendige syntaksen for et gitt uttrykk. Det er dette
;; som gjør at du kan "bruke" special forms som variabel navn samtidig som du bruker
;; det som en faktisk special form.

;; Oppg. 2
;; (a)
;;
;; Endret prosedyre i evaluator.scm
 ;; (define primitive-procedures
 ;;  (list (list 'car car)
 ;; 	(list 'cdr cdr)
 ;; 	(list 'cons cons)
 ;; 	(list 'null? null?)
 ;; 	(list 'not not)
 ;; 	(list '+ +)
 ;; 	(list '- -)
 ;; 	(list '* *)
 ;; 	(list '/ /)
 ;; 	(list '= =)
 ;; 	(list 'eq? eq?) 
 ;; 	(list 'equal? equal?)
 ;; 	(list 'display
 ;; 	      (λ (x) (display x) 'ok))
 ;; 	(list 'newline
 ;; 	      (λ () (newline) 'ok))
 ;; 	;;      her kan vi legge til flere primitiver.
 ;; 	(list '1+
 ;; 	      (λ (x) (+ x 1)))
 ;; 	(list '1-
 ;; 	      (λ (x) (- x 1)))
;; 	        ))
;;
;; (b)

(define (install-primitive! exp proc)
  (define-variable! exp (list 'primitive proc) the-global-environment))

;; Oppg. 3
;; (a)
;;
;; Står øverst i evaluator.scm
;;
;; (b)
;;
;; Står øverst i evaluator.scm
;; Vi har også endret kallet i eval-special-form til å
;; kallet på if-chooser dersom (if? exp), slik at vi beholder den originale eval-if.
;;
;; (c)
;;
;; Står øverst i evaluator.scm
;; Vi har også gjort endringer i special-form? og eval-special-form
;;
;; (d)
;;
;; Står øverst i evaluator.scm
;;
;; (e)
;;
;; Ikke gjort grunnet eksamen

