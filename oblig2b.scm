(load "oblig2b_tester.scm")

;; 
;; Oblig 2b, INF2810 v15
;; Mathias Källström (mathiapk) og Stian Kongsvik (stiako)

;; Oppg. 1
;; (a)

(define (make-counter)
  (define count 0)
    (lambda ()
      (begin (set! count (+ count 1))
	     count)))
;;
;; (b)

;; Oppg. 2
;; (a)

(define (make-stack stack)
  (define (push! list)
    (set! stack (append (reverse list) stack)))
  (define (pop!)
    (if (not (null? stack))
    (set! stack (cdr stack))))
  (define (pass-message message . r)
      (cond ((eq? message 'push!) (push! r))
	    ((eq? message 'pop!) (pop!))
	    ((eq? message 'stack) stack)))
  pass-message)

;; (b)

(define (pop! stack)
  (stack 'pop!))

(define (stack stack-object)
  (stack-object 'stack))

(define (push! stack-object . r)
  (apply stack-object (cons 'push! r)))
  
;; Oppg. 3
;; (a)

;;Boks-og-peker diagram etter kallet på define:

;;+---+---+    +---+---+    +---+---+    +---+---+    +---+XXX+
;;|   |   +----+   |   +----+   |   +----+   |   +----+   |XXX|
;;+-+-+---+    +-+-+---+    +-+-+---+    +-+-+---+    +-+-+XXX+
;;  |            |            |            |            |      
;;  |            |            |            |            |      
;;+-+-+        +-+-+        +-+-+        +-+-+        +-+-+    
;;| a |        | b |        | c |        | d |        | e |    
;;+---+        +---+        +---+        +---+        +---+   

;;Etter kallet på set-cdr!

;;               +-----------------------------+               
;;               |                             |               
;;+---+---+    +-+-+---+    +---+---+    +---+-+-+   
;;|   |   +----+   |   +----+   |   +----+   |   |   
;;+-+-+---+    +-+-+---+    +-+-+---+    +-+-+---+   
;;  |            |            |            |              
;;  |            |            |            |                
;;+-+-+        +-+-+        +-+-+        +-+-+        
;;| a |        | b |        | c |        | d |        
;;+---+        +---+        +---+        +---+         

;; list-ref returnerer symbolet i en liste på en gitt indeks.
;; I dette eksempelet vil list-ref kall med indeks 0-3 gi henholdsvis a-d.
;; Men fordi cdddr (d e) blir satt til å peke på cdr (b c d e) blir e "borte".
;; Dette er fordi hver gang vi kommer til d i lista vil den peke tilbake til b, 
;; og vi kommer aldri til e. Dermed får vi at et kall på list-ref med indeks 1, 4, 7, 10 (++3)
;; vil alle returnere b.

;; (b)

;;Før kallet på set-car!:

;;+---+---+    +---+---+    +-------+
;;|   |   +----+   |   +----+   |XXX|
;;+-+-+---+    +-+-+---+    +-+-----+
;;  |            |            |      
;;  |            |            |      
;;+-+---+      +-+-+        +-+---+  
;;|bring|      | a |        |towel|  
;;+-----+      +---+        +-----+  

;;Etter kallet på set-car!:

;;  +------------+                   
;;  |            |                   
;;+-+-+---+    +-+-+---+    +-------+
;;|   |   +----+   |   +----+   |XXX|
;;+---+---+    +-+-+---+    +-+-----+
;;               |            |      
;;               |            |      
;;             +-+-+        +-+---+  
;;             | a |        |towel|  
;;             +---+        +-----+  


;; Etter første kallet på set-car! får vi en nested list. 
;; Når vi da kaller set-car! på den indre listen i bah (car bah) 
;; setter vi kun car av den indre listen til å peke på 42, 
;; ikke car av bah. Fordi "begge listene" her peker på samme liste
;; vil car i begge endres til å peke på 42.

;; (c)

(define (cycle? lst)
  (define (list-iter rest-list counted)
    (cond ((null? rest-list) #f)
	  ((memq rest-list counted) #t)
	  (else (list-iter (cdr rest-list) (cons rest-list counted)))))
  (list-iter lst '()))

;; (d)

;; Definisjonen av en liste i scheme sier at den er endelig
;; og termineres av den tomme listen. Dermed vil verken sirkulære lister
;; eller par bli oppfattet som lister ved bruk av predikatet "list?".

;; (e)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (last-ring-pair v lst)
  (if (eq? v (cdr lst)) lst
      (last-ring-pair v (cdr lst))))

(define (backup lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (backup (cdr lst)))))

(define (make-ring list)
  (let ((x (backup list)))
    (set-cdr! (last-pair x) x)
    (define (top)
      (car x))
    (define (left-rotate!)
      (set! x (cdr x)))
    (define (reverse!)
      (define (loop x y)
	(if (not (null? x))
	    (let ((temp (cdr x)))
	      (set-cdr! x y)
	      (loop temp x))))
        (loop x '())
	)
    (define (right-rotate!)
      (reverse!)(left-rotate!)(reverse!))
    (define (delete!)
      (set-car! x (cadr x))
      (set-cdr! (last-ring-pair (cdr x) (cdr x)) (cddr x)))
    (define (insert! elm)
      (set-cdr! x (cons (car x) (cdr x)))
      (set-car! x (car elm)))
    (define (show) x)
    (define (pass-message message . r)
      (cond ((eq? message 'top) (top))
	    ((eq? message 'show) (show))
	    ((eq? message 'left-rotate!) (left-rotate!))
	    ((eq? message 'right-rotate!) (right-rotate!))
	    ((eq? message 'delete!) (delete!) (top))
	    ((eq? message 'reverse!) (reverse!) x)
	    ((eq? message 'insert!) (insert! r) (top))))
    pass-message))

(define (top ring)
  (ring 'top))

(define (show ring)
  (ring 'show))

(define (reverse! ring)
  (ring 'reverse!))

(define (left-rotate! ring)
  (ring 'left-rotate!)(top ring))

(define (right-rotate! ring)
  (ring 'right-rotate!)(top ring))

(define (insert! ring elm)
  (ring 'insert! elm))

(define (delete! ring)
  (ring 'delete!))

;; (f)
;; Implementasjonen av left-rotate! bruker konstant tid, og er lite kompleks (O(k)).
;; right-rotate! er linæer men gjør to reverse-rekursjoner, og blir da O(2n)
