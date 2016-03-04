;; (require r5rs)
;; (print-as-expression #f)
;; (print-mpair-curly-braces #f)
(load "prekode3a.scm")

;; Stian Kongsvik (stiako) & Mathias Källström (mathiapk)
;; Oblig 3a

;; Oppgave 1
;; (a & b)
(define original-table (make-table))

(define (mem call proc)
  (let ((table (make-table)))
    (define (memoize)
      (let ((new-proc
	     (lambda args
	       (let ((alrdy-computed (lookup args table)))
		 (or alrdy-computed
		     (let ((result (apply proc args)))
		       (insert! args result table)
		       result))))))
	(insert! new-proc proc original-table)
	new-proc))
    (define (unmemoize)
      (lookup proc original-table))
    (cond ((equal? call 'memoize)(memoize))
	  ((equal? call 'unmemoize)(unmemoize))
	  (else (display "Invalid procedure call")))))


;; (c)
;; Hovedproblemet her er at mem-fib kun memoiserer resultatet til selve kall-argumentet,
;; men ikke "resten" av tallene som brukes for å kalkulere resultatet. Dette skjer fordi
;; det brukes "define" fremfor "set!". Når man bruker "define" så endres man ikke det
;; fib spesifikt peker på, og derfor blir kun selve resultatet til kall-argumentet lagret
;; fremfor resultatet ved hver iterasjon.

;; (d)

(define (arg-finder key args)
  (cond ((null? args) '())
	((equal? key (car args))(cadr args))
	(else (arg-finder key (cddr args)))))

(define (greet . args)
  (define time (arg-finder 'time args))
  (if (null? time) (set! time "day"))
  (define title (arg-finder 'title args))
  (if (null? title) (set! title "friend"))
  (display "good ")(display time) (display " ") (display title)
  (newline))
    
;; Oppg. 2
;; (a)

(define (list-to-stream list)
  (if (null? list)'()
      (cons-stream (car list) (list-to-stream (cdr list)))))

(define (stream-to-list stream . args)
  (define (stream-iter x end)
    (cond ((= end 0)'())
	  ((null? x)'())
	  (else (cons (stream-car x)
		      (stream-iter (stream-cdr x) (- end 1))))))
  (stream-iter stream (if (null? args) 15 (car args))))

;; (b)

(define (stream-map proc . argstreams)
  (if (check-for-empty? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(define (check-for-empty? streams)
  (cond ((null? streams) #f)
	((stream-null? (car streams)) #t)
	(else (check-for-empty? (cdr streams)))))

;; (c)
;; Om vi bare bytter ut prosedyrekallene med stream-versjonene vil vi få problemer
;; når vi kommer med en uendelig strøm, fordi den aldri vil stoppe. En løsning kan
;; være å ha en eq?-sjekk for å stoppe rekursjonen, eller bruke stream-interval.

;; Et annet potensielt problem er at når den møter på duplikater vil den fjerne 
;; forrige funn av elementet og legge til det nye funnet bakerst i strømmen.
;; Dette kan føre til at dersom du har en strøm (1 2 3 4 5 4 3 2 1) vil du ende opp
;; med strømmen (5 4 3 2 1). Dette er ikke nødvendigvis et problem dersom rekkefølge
;; er urelevant.
;; (d)

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
		   (remove-duplicates
		    (stream-filter
		     (lambda (x) 
		       (not (eq? x (stream-car stream)))) 
		     (stream-cdr stream))))))


;; (e)

;;(define x
;;(stream-map show
;;(stream-interval 0 10)))

;; Her vises 0. Dette er fordi show kun skriver ut elementet den får inn. Fordi den får
;; en stream som begynner på 0 er dette det eneste elementet i streamen den har tilgangen til.
;; Derfor er det også det som blir skrevet ut. Hadde det derimot vært en liste ville den skrevet ut
;; hele listen.

;; (stream-ref x 5)

;; Her skriver det ut: 1 2 3 4 5, og deretter blir 5 returnert fra "stream-ref".
;; Dette er fordi show vil bli kjørt for hver rekursjon som skjer i "stream-ref".
;; Og fordi "stream-ref" får cdr av streamen ved hver rekursjon vil car være
;; et nytt tall hver gang show kjører.

;; (stream-ref x 7)

;; Her skrives det ut: 6 7, og deretter blir 7 returnert. Grunnen til at det her
;; kun skrives ut 6 og 7 er fordi resultatene til og med 5 er memoisert og det
;; ligger i minnet hvilke resultat som ble gitt for tallene til og med 5.
;; Derfor vil ikke "display" i show slå ut for disse tallene, fordi den faktisk ikke
;; blir kjørt for disse verdiene. 

;; (f)

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams nats factorials)))

