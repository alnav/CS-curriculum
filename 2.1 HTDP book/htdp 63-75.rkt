;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 63-75|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; HTDP

;; Exercise 63
; Evaluate the following expressions:
(define (distance-to-0 ap)
  (sqrt
    (+ (sqr (posn-x ap))
       (sqr (posn-y ap)))))

(distance-to-0 (make-posn 3 4))

(distance-to-0 (make-posn 6 (* 2 4)))

(+ (distance-to-0 (make-posn 12 5)) 10)

; by hand. Show all steps.
; Assume that sqr performs its computation in a single step.
; Check the results with DrRacket’s stepper.



;; Exercise 64
; The Manhattan distance of a point to the origin considers a path that
; follows the rectangular grid of streets found in Manhattan. Here are two examples:


; The left one shows a “direct” strategy, going as far left as needed,
; followed by as many upward steps as needed.
; In comparison, the right one shows a “random walk” strategy, going some blocks leftward,
; some upward, and so on until the destination—here, the origin—is reached.
; Stop! Does it matter which strategy you follow?

; Design the function manhattan-distance, which measures the Manhattan distance of the given posn to the origin.
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn 1 3)) 4)

(define (manhattan-distance p)
  (+ (posn-x p) (posn-y p)))

;; Exercise 65 / 66
; Take a look at the following structure type definitions:
(define-struct movie [title producer year])

(define-struct person [name hair eyes phone])

(define-struct pet [name number])

(define-struct CD [artist title price])

(define-struct sweater [material size producer])

; Write down the names of the functions (constructors, selectors, and predicates) that each introduces.

;; 66. Revisit the structure type definitions of exercise 65.
; Make sensible guesses as to what kind of values go with which fields.
; Then create at least one instance per structure type definition.

;(define-struct movie [title producer year])
;(define GODFATHER (make-movie "Godfather" "Warner Bros" "1985"))
;(movie-title GODFATHER)
;(movie? GODFATHER)

;; Exercise 67
; Here is another way to represent bouncing balls:
(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")
; Interpret this code fragment and create other instances of balld.

(make-balld 40 "down")

;; Exercise 68
; An alternative to the nested data representation of balls uses four fields to keep track of the four properties:
; Yet another alternative is to use complex numbers.
; If you know about them, contemplate a data representation that uses them for both location and velocity.

; For example, in BSL, 4-3i is a complex number and could be used to represent the location or velocity (4,-3).

(define-struct ballf [x y deltax deltay])
; Programmers call this a flat representation. Create an instance of ballf that has the same interpretation as ball1)
#;
(define ball1
  (make-ball (make-posn 30 40) (make-vel -10 5)))

(define ball1f (make-ballf 30 40 -10 5))



;; Exercise 69
; Draw box representations for the solution of exercise 65.
; ...

;; Exercise 70
;  Spell out the laws for these structure type definitions:
(define-struct centry [name home office cell])
(define-struct phone [area number])

;; Exercise 71
; Place the following into DrRacket’s definitions area:
; distances in terms of pixels:
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH  400)
(define CENTER (quotient WIDTH 2))
 
(define-struct game [left-player right-player ball])
 
(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))
; Click RUN and evaluate the following expressions:
(game-ball game0)
(posn? (game-ball game0))
(game-left-player game0)
; Explain the results with step-by-step computations. Double-check your computations with DrRacket’s stepper.


;; Exercise 72
; Formulate a data definition for the above phone structure type definition that accommodates the given examples.
; (define-struct phone [area number])

; Next formulate a data definition for phone numbers using this structure type definition:
(define-struct phone# [area switch num])

; Historically, the first three digits make up the area code,
; the next three the code for the phone switch (exchange) of your neighborhood,
; and the last four the phone with respect to the neighborhood.

; Describe the content of the three fields as precisely as possible with intervals.

; (define-struct phone [area number])

; A Phone is a structure: 
;   (make-phone Number Number)
; interpretation (make-phone area number) means a phone which has:
; an area prefix
; a number

; (define-struct phone# [area switch num])
; A Phone# is a structure: 
;   (make-phone# Number[0,999] Number[0,999] Number[0,9999])
; interpretation (make-phone# area switch num) means a phone which has:
; an area prefix [3 digits]
; a switch number [3 digits]
; a number [4 digits]


;; Exercise 73
; Design the function posn-up-x, which consumes a Posn p and a Number n.
; It produces a Posn like p with n in the x field.

;; Exercise 74
; Copy all relevant constant and function definitions to DrRacket’s definitions area.
; Add the tests and make sure they pass. Then run the program and use the mouse to place the red dot.

; (main (make-posn 0 50))

(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))
 
; A Posn represents the state of the world.
 
; Posn -> Posn 
(define (main p0)
  (big-bang p0
    [on-tick x+]
    [on-mouse reset-dot]
    [to-draw scene+dot]))

; Posn -> Image
; adds a red spot to MTS at p
(check-expect (scene+dot (make-posn 10 20))
              (place-image DOT 10 20 MTS))
(check-expect (scene+dot (make-posn 88 73))
              (place-image DOT 88 73 MTS))

(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

; Posn -> Posn
; increases the x-coordinate of p by 3
(check-expect (x+ (make-posn 10 0)) (make-posn 13 0))

(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

;; Posn Number -> Posn
;; Produces a Posn, with posn-x = n
(check-expect (posn-up-x (make-posn 4 5) 6) (make-posn 10 5))

(define (posn-up-x p n) (make-posn (+ (posn-x p) n) (posn-y p)))

; Posn Number Number MouseEvt -> Posn 
; for mouse clicks, (make-posn x y); otherwise p
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-down")
  (make-posn 29 31))
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-up")
  (make-posn 10 20))

(define (reset-dot p x y me)
  (cond
    [(mouse=? me "button-down") (make-posn x y)]
    [else p]))

;; Exercise 75
; Enter these definitions and their test cases into the definitions area of DrRacket and make sure they work.
; This is the first time that you have dealt with a “wish,” and you need to make sure you understand how the two functions work together. 

(define-struct vel [deltax deltay])
; A Vel is a structure: 
;   (make-vel Number Number)
; interpretation (make-vel dx dy) means a velocity of 
; dx pixels [per tick] along the horizontal and
; dy pixels [per tick] along the vertical direction

(define-struct ufo [loc vel])
; A UFO is a structure: 
;   (make-ufo Posn Vel)
; interpretation (make-ufo p v) is at location
; p moving at velocity v

(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))

(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))
 
(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

; UFO -> UFO
; determines where u moves in one clock tick; 
; leaves the velocity as is
 
(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2)
              (make-ufo (make-posn 17 77) v2))
 
(define (ufo-move-1 u)
  (make-ufo (posn+ (ufo-loc u) (ufo-vel u))
            (ufo-vel u)))

; Posn Vel -> Posn 
; adds v to p 
(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))





























