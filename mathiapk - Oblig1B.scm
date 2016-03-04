;INF2810 - Oblig 1b
;Mathias Källström (mathiapk)
;27.02.15

;Oppgave 1
;(a)

;+-------+-------+      +-------+
;|       |       |      |       |
;|       |      +-------+   11  |
;|   +   |       |      |       |
;+-------+-------+      +-------+
;    |                           
;    |                           
;+---+---+                       
;|       |                       
;|   47  |                       
;|       |                       
;+-------+  

;(b)

;+-------+-------+     +-------+
;|       |       |     |XXXXXXX|
;|       |       +-----+XXXXXXX|
;|   +   |       |     |XXXXXXX|
;+-------+-------+     +-------+
;    |                          
;    |                          
;+---+---+                      
;|       |                      
;|   47  |                      
;|       |                      
;+-------+                      

;(c)

;+------+-------+       +--------+--------+
;|      |       |       |        |XXXXXXXX|
;|      |      +--------+        |XXXXXXXX|
;|      |       |       |        |XXXXXXXX|
;+---+--+-------+       +----+---+--------+
;    |                       |             
;    |                       |             
;+---+--+               +----+---+         
;|      |               |        |         
;|  47  |               |    11  |         
;|      |               |        |         
;+------+               +--------+         

;(d)

;+------+-------+        +-------+-------+                       
;|      |       |        |       |XXXXXXX|                       
;|      |       +--------+       |XXXXXXX|                       
;|      |       |        |       |XXXXXXX|                       
;+---+--+-------+        +---+---+-------+                       
;    |                       |                                   
;    |                       |                                   
;    |                       |                                   
;+---+--+                +---+---+-------+      +-------+-------+
;|      |                |       |       |      |       |XXXXXXX|
;|   47 |                |       |       +------+       |XXXXXXX|
;|      |                |       |       |      |       |XXXXXXX|
;+------+                +---+---+-------+      +---+---+-------+
;                            |                      |            
;                            |                      |            
;                            |                      |            
;                        +---+---+              +---+---+        
;                        |       |              |       |        
;                        |   11  |              |   12  |        
;                        |       |              |       |        
;                        +-------+              +-------+        

;(e)

;+----+----+    +----+----+    +----+----+    +----+----+
;|    |    +----+    |    +----+    |    +----+    |XXXX|
;|    |    |    |    |    |    |    |    |    |    |XXXX|
;+--+-+----+    +--+-+----+    +--+-+----+    +--+-+----+
;   |              |              |              |       
;   |              |              |              |       
;   |           +--+-+         +--+-+         +--+-+     
;   |           | 1  |         | 2  |         | 3  |     
;   |           |    |         |    |         |    |     
;   |           +----+         +----+         +----+     
;   |                                                    
;   |                                                    
;+--+-+----+    +----+----+    +----+----+               
;|    |    +----+    |    +----+    |XXXX|               
;|    |    |    |    |    |    |    |XXXX|               
;+--+-+----+    +--+-+----+    +--+-+----+               
;   |              |              |                      
;   |              |              |                      
;+--+-+         +--+-+         +--+-+                    
;| 1  |         | 2  |         | 3  |                    
;|    |         |    |         |    |                    
;+----+         +----+         +----+                    

;(f)
(define list1 (list 1 2 3 4))
list1
(car(cdr(cdr list1)))

;(g)
(define list2 (list (list 1 2) (list 3 4)))
list2
(car(car(cdr list2)))

;(h)
(define list3 (list (list 1) (list 2) (list 3) (list 4)))
list3
(car(car(cdr(cdr list3))))

;(i)
;Med cons
(cons(cons 1(cons 2'()))(cons(cons 3(cons 4 '()))'()))
;Med list
(define listi (list (list 1 2) (list 3 4)))

;OPPGAVE 2
;(a)
(define (length2 items)
  (define(test a ut)
    (if (null? ut)
        a
        (test(+ a 1)(cdr ut))))
  (test 0 items))

;(b)
(define (rev-list items)
  (define (inner in out)
    (if (null? in) 
        out
        (inner (cdr in)(cons (car in) out))))
  (inner items '()))
(rev-list '("towel" "a" "bring" "always"))
;Her benytter jeg halerekursjon. Dette er fordi en slik halerekursiv prosedyre
;"automatisk" reverserer listen. Dette skyldes at mens den "originale listen" gås gjennom fra start til
;slutt, bygges den nye listen opp baklengs. Det vil si at når jeg conser "out" med "car in" blir "car in"
;satt på indeks 0 i den nye listen. Litt upresis forklaring, men håper det gir mening.

;(c)
(define(ditch a items)
  (define(check in out)
    (if(null? in)
       out
    (cond((= a (car in))
       (check (cdr in) out))
       (else(check (cdr in)(cons (car in) out))))))
  (check items '()))
;Skrev først en halerekursiv prosedyre, men fant raskt ut at jeg ender opp med en reversert liste.
;Skrev derfor en prosedyre hvor jeg brukte vanlig rekursjon i tillegg.
(define(ditch2 a items)
  (cond((null? items)'())
       ((= a (car items))
        (ditch2 a (cdr items)))
       (else(cons (car items)(ditch2 a (cdr items))))))

;(d)

(define (nth a items)
  (define(inner b in)
    (if(= b a)
       (car in)
       (inner (+ b 1)(cdr in))))
  (inner 0 items))

;(e)

(define (where a items)
  (define(inner b in)
    (if(null? in)
       #f
    (if(= a (car in))
       b
       (inner (+ b 1)(cdr in)))))
  (inner 0 items))
     
;(f)
;Synes det var uklart her om prosedyren kun skulle plusse elementene, 
;eller om det skulle være mulig å bruke et hvilket som helst predikat.
;Jeg valgte sistenevnte.
(define (map2 pred list1 list2)
  (cond ((null? list1)'())
        ((null? list2)'())
        (else(cons(pred(car list1)(car list2))
                  (map2 pred (cdr list1)(cdr list2))))))

;(g)
;Bruker anonym prosedyre for å regne gjennomsnitt av elementer på samme indeks i to ulike lister.
(define (map3 list1 list2)
  (cond ((null? list1)'())
        ((null? list2)'())
        (else(cons((lambda (x y)(/ (+ x y) 2))(car list1)(car list2))
                  (map3 (cdr list1)(cdr list2))))))
;Bruker anonym prosedyre for å sjekke om elementer på samme indeks i to ulike lister
;er partall. Da conser den #t. Hvis minst et av tallene er oddetall conser den #f.
(define (map4 list1 list2)
  (cond ((null? list1)'())
        ((null? list2)'())
        (else(cons((lambda (x y)(and(even? x)(even? y)#t))(car list1)(car list2))
                  (map3 (cdr list1)(cdr list2))))))
;(h)

(define (both? pred)
  (lambda(x y)
    (and(pred x)(pred y))))
  
;(i)
(define (self pred)
  (lambda (x)(pred x x)))