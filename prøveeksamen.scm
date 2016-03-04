(load "prekode3a.scm")

;;Oppgave 1

;; Her er siste kallet på foo ((a 7) (a 7))

;;Oppgave 2

;; Prosedyren x utfører en prosedyre y på alle elementer 
;; i en liste z.

;; Kalleksempel: (x list? '((1 2 3)(1 2 3)(1 2 3)))
;; returverdi: #t

;;Oppgave 3

(define (avg first . rest)
  (let ((count 0)(sum 0))
    (define (rekavg nmbrs)
      (if (null? nmbrs)
          (/ sum count)
          (begin(set! count (+ count 1))
                (set! sum (+ sum (car nmbrs)))
                (rekavg (cdr nmbrs)))))
    (rekavg (append (list first) rest))))

;;Oppgave 4

;; Omgivelsesdiagram

;;Oppgave 5

(define (transform-if pred proc lst)
  (define (rek rest-list)
    (cond ((null? rest-list) '())
          ((pred (car rest-list))
           (cons(proc (car rest-list))
                (rek (cdr rest-list))))
          (else (cons (car rest-list) (rek (cdr rest-list))))))
    (rek lst))

(define (transform-if! pred proc lst)
  (define (rek rest-list)
    (cond ((null? rest-list) lst)
          ((pred (car rest-list))
           (set-car! rest-list (proc (car rest-list)))
           (rek (cdr rest-list)))
          (else (rek (cdr rest-list)))))
  (rek lst))
           

;; Oppgave 6

;; Eager/applicative-order evaluation evaluerer hele uttrykket med 
;; en gang. Det vil si at alle variabler og prosedyrer må være definert
;; før det kjøres. Lazy/Normal-order evaluation evaluerer kun det den 
;; trenger for å utføre operasjonen. Alt annet blir satt på "vent". 
;; Det kan sammenlignes med lister og strømmer, hvor lister benytter
;; applicative-order evaluation, mens strømmer benytter normal-order.
;; Scheme bruker applicative-order som standard, med mindre koden
;; benytter såkalte "special-forms" som feks "if". 

;; Oppgave 7

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (fringe (car tree))
                 (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))


(define (same-fringe? tree1 tree2 pred)
  (define (iter lst1 lst2)
    (cond ((and (null? lst1) (null? lst2))#t)
          ((pred (car lst1) (car lst2))
           (iter (cdr lst1) (cdr lst2)))
          (else #f)))
  (iter (fringe tree1) (fringe tree2)))
  

;; (b)
  
(define (fringe-stream tree)
  (cond ((stream-null? tree) '())
        ((pair? (car tree))
         (stream-append (fringe-stream (car tree))
                        (fringe-stream (cdr tree))))
        (else (cons-stream (car tree)
                           (fringe-stream (cdr tree))))))
  
(define (same-fringe-stream? tree1 tree2 pred)
  (define (iter stream1 stream2)
    (cond ((and (stream-null? stream1)
                (stream-null? stream2))#t)
          ((or (stream-null? stream1)
               (stream-null? stream2))#f)
          ((pred (stream-car stream1)
                 (stream-car stream2))
           (iter (stream-cdr stream1)
                 (stream-cdr stream2)))
          (else #f)))
  (iter (fringe-stream tree1) 
        (fringe-stream tree2)))

;; (c)

;; Det første kallet vil gi 1 cons-celle.
;; Dette er fordi "cons-stream" kallet automatisk gir en cons-celle.

;; Det andre kallet gir 3 cons-celler. Dette er fordi "pair?"-testen
;; slår ut med en gang og oppretter da en cons-celle ved kallet på
;; stream-append, som videre oppretter en cons-celle per kall på
;; fringe-stream.

;; (d)

;; Oppgave 8


(define (monitor procedure)
  (let ((count 0))
    (lambda arguments
      (let ((message (and (not (null? arguments))
                          (car arguments))))
        (cond ((eq? message 'zero)(set! count 0))
              ((eq? message 'count)count)
              ((eq? message 'reset)procedure)
              (else (set! count (+ 1 count))
                    (apply procedure arguments)))))))