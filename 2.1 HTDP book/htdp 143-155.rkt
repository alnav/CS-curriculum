;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 143-155|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Exercise 143.
;Determine how average behaves in DrRacket when applied to the empty list.
;Then design checked-average, a function that produces an informative error message when it is applied to '().
(define t1 (cons 5 (cons 2 (cons 9 (cons 4 '())))))

(define (sum alot)
  (cond
  [(empty? alot) 0]
  [else (+ (first alot) (sum (rest alot)))]))


(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ 1 (how-many (rest alot)))]))


(define (average alot)
  (/ (sum alot) (how-many alot)))

(define (checked-average alot)
  (cond
    [(empty? alot) (error "checked-average: input must be a non-empty list")]
    [else (average alot)]))


;; Exercise 144. Will sum and how-many work for NEList-of-temperatures even though they are designed for inputs from List-of-temperatures?
;If you think they don’t work, provide counter-examples. If you think they would, explain why.



;; Exercise 145. Design the sorted>? predicate, which consumes a NEList-of-temperatures and produces
;#true if the temperatures are sorted in descending order.
;That is, if the second is smaller than the first, the third smaller than the second, and so on.
;Otherwise it produces #false.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

#;
(define (sorted>? list) ...)

;NEList-of-temperatures -> Boolean
(check-expect (sorted>? (cons 8 (cons 5 (cons 3 '())))) #true)
(check-expect (sorted>? (cons 8 (cons 5 (cons 8 '())))) #false)
(check-expect (sorted>? (cons 3 '())) #true)
(check-expect (sorted>? (cons 10 (cons 2 '()))) #true)

(define (sorted>? list)
  (cond
    [(empty? (rest list)) #true]
    [(and (> (first list) (first (rest list))) 
          (sorted>? (rest list))) #true]
    [else #false]))



;; Exercise 146.
;Design how-many for NEList-of-temperatures.
;Doing so completes average, so ensure that average passes all of its tests, too.
(check-expect (how-manyN (cons 3 (cons 4 '()))) 2)
(check-expect (how-manyN (cons 3 '())) 1)
(check-expect (how-manyN (cons 1 (cons 4 (cons 4 '())))) 3)

(define (how-manyN NEList)
  (cond
    [(empty? (rest NEList)) 1]
    [else (+ 1 (how-many (rest NEList)))]))

;; Exercise 147.
;Develop a data definition for NEList-of-Booleans, a representation of non-empty lists of Boolean values.
;Then redesign the functions all-true and one-true from exercise 140.

; - NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Boolean)

; lob -> Boolean
; consumes a NElist-of-boolean, return #true if all of them are true
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)

(define (all-true lob)
  [cond
  [(empty? (rest lob))
   (first lob)]
  [else (and (first lob) (all-true (rest lob)))]])

;; Exercise 148.
;Compare the function definitions from this section (sum, how-many, all-true, one-true)
;with the corresponding function definitions from the preceding sections.
;Is it better to work with data definitions that accommodate empty lists
;as opposed to definitions for non-empty lists? Why? Why not?

; ...

;; Copier function
; N String -> List-of-strings
; creates a list of n copies of s

(check-expect (copier 0 "hi") '())
(check-expect (copier 2 "hi") (cons "hi" (cons "hi" '())))

(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))


;; Exercise 149.
;Does copier function properly when you apply it to a natural number
;and a Boolean or an image? Or do you have to design another function?
;Read Abstraction for an answer.


;; Exercise 150.
;Design the function add-to-pi.
;It consumes a natural number n and adds it to pi without using the primitive + operation.

;Once you have a complete definition, generalize the function to add,
;which adds a natural number n to some arbitrary number x without using +.
;Why does the skeleton use check-within?

; Natural -> Number
(check-within (add-to-pi 3) (+ 3 pi) 0.001)

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; Natural Number -> Number
(check-expect (add-num 3 5) 8)
(check-expect (add-num 0 123) 123)

(define (add-num n n2)
  (cond
    [(zero? n) n2]
    [(positive? n) (add1 (add-num (sub1 n) n2))]))

;;Exercise 151.
;Design the function multiply.
;It consumes a natural number n and multiplies it with a number x without using *.

; Natural Number -> Number
(check-expect (multiply 1 5) 5)
(check-expect (multiply 0 4) 0)
(check-expect (multiply 2 3) 6)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(positive? n) (+ x (multiply (sub1 n) x))]))

;; Exercise 152. Design two functions: col and row.
;The function col consumes a natural number n and an image img.
;It produces a column—a vertical arrangement—of n copies of img.
;The function row consumes a natural number n and an image img.
;It produces a row—a horizontal arrangement—of n copies of img.

(define SQ (square 100 "solid" "red"))

; Natural Image -> Image
(check-expect (col 3 SQ) (above SQ (above SQ SQ)))
(check-expect (col 1 SQ) SQ)

(define (col n img)
  (cond
    [(= n 1) img]
    [else (above img (col (sub1 n) img))]))

; Natural Image -> Image
(check-expect (row 3 SQ) (beside SQ (beside SQ SQ)))
(check-expect (row 1 SQ) SQ)

(define (row n img)
  (cond
    [(= n 1) img]
    [else (beside img (row (sub1 n) img))]))


;; Exercise 153.
;The goal of this exercise is to visualize the result of a 1968-style European student riot. Here is the rough idea.
;A small group of students meets to make paint-filled balloons, enters some lecture hall,
;and randomly throws the balloons at the attendees.
;Your program displays how the balloons color the seats in the lecture hall.

;Use the two functions from exercise 152 to create a rectangle of 8 by 18 squares,
;each of which has size 10 by 10. Place it in an empty-scene of the same size. This image is your lecture hall.

;Design add-balloons. The function consumes a list of Posn whose coordinates fit into the dimensions of the lecture hall.
;It produces an image of the lecture hall with red dots added as specified by the Posns.

(define BOX (overlay (square 8 "solid" "white") (square 10 "solid" "black")))

(define BG (overlay (row 8 (col 18 BOX)) (empty-scene 80 180)))

(define DOT (circle 5 "solid" "red"))

; List-of-Posn -> Image
(check-expect (add-balloons (cons (make-posn 3 5) '())) (place-image DOT (* 10 3) (* 10 5) BG))
(check-expect (add-balloons (cons (make-posn 4 2) (cons (make-posn 7 16) '()))) (place-image DOT (* 10 4) (* 10 2) (place-image DOT (* 10 7) (* 10 16) BG)))

(define (add-balloons p)
  (cond
    [(empty? p) BG]
    [else (place-image DOT (* 10 (posn-x (first p))) (* 10 (posn-y (first p))) (add-balloons (rest p)))]))

;; Exercise 154.
;Design the function colors. It consumes a Russian doll and produces a string of all colors,
;separated by a comma and a space. Thus our example should produce "yellow, green, red"

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)
(define RD1 (make-layer "green" (make-layer "red" "orange")))
(define RD2 (make-layer "yellow" "black"))
(define RD3 "blue")


; RD -> String
(check-expect (colors RD1) "green, red, orange")
(check-expect (colors RD2) "yellow, black")
(check-expect (colors RD3) "blue")

(define (colors rd)
  (cond
    [(string? rd) rd]
    [else (string-append (string-append (layer-color rd) ", ") (colors (layer-doll rd)))]))


;; Exercise 155. Design the function inner, which consumes an RD and produces the (color of the) innermost doll.
;Use DrRacket’s stepper to evaluate (inner rd) for your favorite rd.

; RD -> String
(check-expect (inner RD3) "blue")
(check-expect (inner RD2) "black")
(check-expect (inner RD1) "orange")

(define (inner rd)
  (cond
    [(string? rd) rd]
    [else (inner (layer-doll rd))]))
