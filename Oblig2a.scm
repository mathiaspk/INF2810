;; Stian Kongsvik (stiako), Mathias Källström (mathiapk)
;; Oblig 2a
;;(require r5rs)
(load "huffman.scm")

;; Oppgave 1
;; (a)

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;
;; (b)

(define foo 30)

((lambda (x y)
   (+ foo x y))
 foo 20)
;; Dette uttrykket evaluerer til 80

((lambda (foo)
 ((lambda (x y)
    (+ foo x y))
  foo 20))
 10)
;; Dette uttrykket evaluerer til 40. I dette uttrykket benytter vi
;; en ny lokal variabel "foo", fremfor den globale vi definerte foerst.
;; Dermed er baade "foo" og "x" lik 10: 10 + 10 + 20 = 40.
;;
;; (c)
;;
;; Resultatet blir '(6 -4 21 1/2).
;;
;; Kallet på map oppretter en ny liste med elementer fra resultatet til lambda uttrykket.
;; Hvert element i den nye listen er et resultat av (y x z), eller (a2 a1 a3).
;;
;; Kallet på map itererer gjennom listene a1-3 der lambda-uttrykket utfører prosedyren i a2 på
;; elementene i a1 og a3 på tilsvarende indeks. lambda-uttrykket gjør at en kan skrive uttrykk med
;; infiksnotasjon:
;;
;; ((λ (x y z) (y x z)) (car a1) (car a2) (car a3))
;; alt. ((λ (x y z) (y x z)) 1 + 5)
;;
;; Oppgave 2
;; (a)

(define (member? elm lst)
  (cond ((null? lst) #f)
	((eq? elm (car lst)) #t)
	(else (member? elm (cdr lst)))))

;; (b)
;; Den ytre decode prosedyren tar i mot en liste med bits (0,1) pluss et Huffmantre.
;; Decode-1 tar i mot resten av listen med bits og den nåværende posisjonen i treet.
;; Grunnen til at vi trenger Decode-1 er for å dele treet opp i subtrær, og for å
;; ha muligheten til å hoppe til toppen av treet når den finner "leaf".

;; (c)

(define (decode-tail-rec bits tree)
  (define (decode-tail-1 bits current-branch finalmsg)
    (if (null? bits)
	finalmsg
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (decode-tail-1 (cdr bits) tree (cons (symbol-leaf next-branch) finalmsg))
	      (decode-tail-1 (cdr bits) next-branch finalmsg)))))
  (reverse (decode-tail-1 bits tree '())))

(decode-tail-rec sample-code sample-tree)

;; (d)

;; Resultatet blir '(ninjas fight ninjas by night). Halerekursive prosedyrer
;; returnerer i utgangspunktet en reversert liste, men ved å bruke "reverse"
;; på listen som halerekursjonen returnerer får vi den riktige beskjeden.
;; Vi kunne i stedet valgt å bruke append, men det hadde økt kompleksiteten
;; kvadratisk, ettersom append må gå gjennom hele listen for hver rekursjon.
;; Med å bruke reverse gjør vi bare en ekstra halerekursjon på lista, som er
;; like mange steg som lengden av lista.

;; (e)      

(define (encode msg tree)
  (if (null? msg)
      '()
      (append (encode-symbol (car msg) tree)
	      (encode (cdr msg) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((current-branch (encode-branch symbol tree)))
	(cons (car current-branch)
	      (encode-symbol symbol (cadr current-branch))))))

(define (encode-branch symbol tree)
  (let ((left (left-branch tree))
	(right (right-branch tree)))
    (cond ((member? symbol (symbols left)) (list 0 left))
	  ((member? symbol (symbols right)) (list 1 right))
	  (else (error "Symbolet finnes ikke i kodeboken")))))

;; (f)

(define (grow-huffman-tree freqs)
  (grow-tree (make-leaf-set freqs)))

(define (grow-tree sorted-set)
  (if (null? (cdr sorted-set))
      (car sorted-set)
      (grow-tree
       (adjoin-set (make-code-tree (left-branch sorted-set)
				   (right-branch sorted-set))
		   (cddr sorted-set)))))

;; (g)

(define samurai-tree (grow-huffman-tree '((ninjas 57) (samurais 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2) (forest 1) (wait 1) (poison 1))))
;; For å kode denne meldingen brukes det 40 bits. Gjennomsnittslengden på hvert kodeord som brukes er 2.5.
;; Det minste antallet tegn for å kode denne meldingen med fast lengde (4 bits) blir 17*4=68.
;; Dette er fordi meldingen inneholder 5 unike tegn, og fire bits er minimum lengde for å
;; generere fem unike bitstrenger.

;; (h)

(define (symbol-weight-list leaf)
  (list (list (symbol-leaf leaf) (weight-leaf leaf))))

(define (huffman-leaves tree)
  (if (leaf? tree)
      (symbol-weight-list tree)
      (append (huffman-leaves (left-branch tree))
	      (huffman-leaves (right-branch tree)))))

;; (i)

;; Traverserer treet kun en gang, deretter utføres avg-codeword-length for hvert
;; symbol og samler alle resultater i en ny liste. Deretter bruker vi reduce for
;; å summere alle svarene.
(define (expected-code-length tree)
  (let ((indiv-result
	 (map (lambda (x y) (avg-codeword-length x tree y))
	      (huffman-leaves tree) (bit-code-list tree))))
    (reduce + 0 indiv-result)))

;; Returnerer vekten til et symbol
(define (symbol-weight pair)
  (cadr pair))

;; Returnerer en ny liste som inneholder bit-koder for alle symbolene i treet
(define (bit-code-list tree)
  (map (lambda (x) (encode-symbol x tree)) (symbols tree)))
	
;; Utfører formelen for relativ frekvens ganget med kodelengde per symbol.
(define (avg-codeword-length pair tree symbol-bits)
  (* (/ (symbol-weight pair) (weight tree))
     (length symbol-bits)))

(define (reduce proc init items)
  (if (null? items)
      init
      (proc (car items)
	    (reduce proc init (cdr items)))))

;; For at denne formelen skal stemme, må den den relative frekvensen per symbol i meldingen stemme overens med den relative frekvensen i treet.
