;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 129-142|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Exercise 129.
;Create BSL lists that represent
;-a list of celestial bodies, say, at least all the planets in our solar system;
;-a list of items for a meal, for example, steak, french fries, beans, bread, water, Brie cheese, and ice cream; and
;-a list of colors.

(define list-planets (cons "mercury" (cons "venus" (cons "earth" '()))))
(define list-meal (cons "bread" (cons "ale" (cons "corn" '()))))
(define list-colours (cons "green" (cons "blue" (cons "yellow" (cons "black" '())))))

;; Exercise 130. 
;Create an element of List-of-names that contains five Strings. 
;Sketch a box representation of the list similar to those found in figure 44.

;Explain why
;(cons "1" (cons "2" '()))
;is an element of List-of-names and why (cons 2 '()) isn’t.

;; Exercise 131. 
;Provide a data definition for representing lists of Boolean values. 
;The class contains all arbitrarily long lists of Booleans.

;list-of-booleans is one of:
; - empty
; - (cons Boolean list-of-boolean)

;; Exercise 132. 
; Use DrRacket to run contains-flatt? in this example:
; (cons "Fagan"
;   (cons "Findler"
;     (cons "Fisler"
;       (cons "Flanagan"
;         (cons "Flatt"
;           (cons "Felleisen"
;             (cons "Friedman" '())))))))
; What answer do you expect? 

;; Exercise 133. 
;Here is another way of formulating the second cond clause in contains-flatt?:
;... (cond
;      [(string=? (first alon) "Flatt") #true]
;      [else (contains-flatt? (rest alon))]) ...
;Explain why this expression produces the same answers as the or expression in the version 
;of figure 47. Which version is clearer to you? Explain. 


;; Exercise 134. 
;Develop the contains? function, which determines whether some given string 
;occurs on a given list of strings.
;
;Note BSL actually comes with member?, a function that consumes two values 
;and checks whether the first occurs in the second, a list:
;> (member? "Flatt" (cons "b" (cons "Flatt" '())))
;#true
;
;Don’t use member? to define the contains? function.

(define (contains? x list)
  (cond
    [(empty? list) #false]
    [(string=? (first list) x) #true]
    [else (contains? x (rest list))]))

;; Exercise 135. 
;Use DrRacket’s stepper to check the calculation for
;(contains-flatt? (cons "Flatt" (cons "C" '())))

;Also use the stepper to determine the value of
;(contains-flatt?
;  (cons "A" (cons "Flatt" (cons "C" '()))))
;What happens when "Flatt" is replaced with "B"? 

;;  Exercise 136. 
;Validate with DrRacket’s stepper
;(our-first (our-cons "a" '())) == "a"
;(our-rest (our-cons "a" '())) == '()
;See What Is '(), What Is cons for the definitions of these functions. 

;; Exercise 137.
;Compare the template for contains-flatt? with the one for how-many.
;Ignoring the function name, they are the same. Explain the similarity. 
; they share the same template

;; Exercise 138. 
;Here is a data definition for representing sequences of amounts of money:
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
;Create some examples to make sure you understand the data definition. 
;Also add an arrow for the self-reference.
;Design the sum function, which consumes a List-of-amounts and computes the sum of the amounts. 
;Use DrRacket’s stepper to see how (sum l) works for a short list l in List-of-amounts

(define loa1 (cons 12 (cons 3 '())))
(define loa2 (cons 12 (cons 3 (cons 51 (cons 50 '())))))

; loa -> Natural
; computes sum of numbers in list-of-amounts
;(define (sum loa) ...)
(check-expect (sum loa1) 15)
(check-expect (sum loa2) 116)

(define (sum loa)
  [cond
    [(empty? loa) 0]
    [+ (first loa) (sum (rest loa))]])

;; Exercise 140. 
;Design the function all-true, which consumes a list of Boolean values 
;and determines whether all of them are #true. 
;In other words, if there is any #false on the list, the function produces #false.

; lob -> Boolean
; consumes a list-of-boolean, return #true if all of them are true
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)

(define (all-true lob)
  [cond
  [(empty? lob) #true]
  [else (and (first lob) (all-true (rest lob)))]])


;Now design one-true, a function that consumes a list of Boolean values 
;and determines whether at least one item on the list is #true. 
(define (or-true lob)
  [cond
  [(empty? lob) #false]
  [else (or (first lob (or-true (rest lob))))]])

;; Exercise 141. 
;If you are asked to design the function cat, which consumes a list of strings 
;and appends them all into one long string, 
;you are guaranteed to end up with this partial definition:

; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
  (cat (cons "ab" (cons "cd" (cons "ef" '()))))
  "abcdef")

#;
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (... (first l) ... (cat (rest l)) ...)]))

(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))

;; Exercise 142. 
;Design the ill-sized? function, which consumes a list of images loi and a positive number n. 
;It produces the first image on loi that is not an n by n square; 
;if it cannot find such an image, it produces #false.

;Hint Use
; ImageOrFalse is one of:
; – Image
; – #false 
;for the result part of the signature.
(define s1 (square 20 "solid" "red"))
(define s2 (square 20 "solid" "red"))
(define s3 (square 40 "solid" "red"))

(define loi1 (cons s1 (cons s2 (cons s3 '()))))

; loi Number -> Image or #false
(check-expect (ill-sized? loi1 20) s3)
(check-expect (ill-sized? loi1 10) s1)

#;
(define (ill-sized? loi n)
  (cond
    [(empty? loi) ...]
    [... (first loi) ... (rest loi)]))

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (if
           (not (and (= (image-height (first loi)) n)
              (= (image-width (first loi)) n)))
          (first loi)
          (ill-sized? (rest loi) n))]))
          