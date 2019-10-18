;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 156-165|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Exercise 156.
;Equip the program in figure 61 with tests and make sure it passes those.
;Explain what main does. Then run the program via main.

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 8 "solid" "red"))

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot 


; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 4 (cons 7 '())))
              (place-image SHOT XSHOTS 4 (place-image SHOT XSHOTS 7 BACKGROUND)))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))




;; Exercise 157.
;Experiment to determine whether the arbitrary decisions concerning constants are easy to change.
;For example, determine whether changing a single constant definition achieves the desired outcome:

;change the height of the canvas to 220 pixels;
;change the width of the canvas to 30 pixels;
;change the x location of the line of shots to “somewhere to the left of the middle”;
;change the background to a green rectangle; and
;change the rendering of shots to a red elongated rectangle.

;yes they are

;Also check whether it is possible to double the size of the shot without changing anything else
;or to change its color to black.

;; Exercise 158.
;If you run main, press the space bar (fire a shot), and wait for a goodly amount of time,
;the shot disappears from the canvas.
;When you shut down the world canvas, however, the result is a world that still contains this invisible shot.

;Design an alternative tock function that doesn’t just move shots one pixel per clock tick
;but also eliminates those whose coordinates place them above the canvas.
;Hint You may wish to consider the design of an auxiliary function for the recursive cond clause.

; ShotWorld -> ShotWorld
(check-expect (tock2 '()) '())
(check-expect (tock2 (cons 3 '())) (cons 2 '()))
(check-expect (tock2 (cons -2 (cons 5 '()))) (cons 4 '()))
(check-expect (tock2 (cons 5 (cons -6 '()))) (cons 4 '()))
(check-expect (tock2 (cons 5 (cons -4 (cons 9 (cons -55 '()))))) (cons 4 (cons 8 '())))

(define (tock2 w)
  (cond
    [(empty? w) '()]
    [(outside? (first w)) (tock2 (rest w))]
    [else (cons (sub1 (first w)) (tock2 (rest w)))]))

; Number -> Boolean
(check-expect (outside? 100) #false)
(check-expect (outside? -1) #true)

(define (outside? n) (< n 0))



;; Exercise 159.
;Turn the solution of exercise 153 into a world program.
;Its main function, dubbed riot, consumes how many balloons the students want to throw;
;its visualization shows one balloon dropping after another at a rate of one per second.
;The function produces the list of Posns where the balloons hit.
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
(define BOX (overlay (square 8 "solid" "white") (square 10 "solid" "black")))

(define BG (overlay (row 8 (col 18 BOX)) (empty-scene 80 180)))

(define DOT (circle 5 "solid" "red"))

; List-of-Posn -> Image
(check-expect (add-balloons (cons (make-posn 3 5) '())) (place-image DOT (* 10 3) (* 10 5) BG))
(check-expect (add-balloons (cons (make-posn 4 2) (cons (make-posn 7 16) '()))) (place-image DOT (* 10 4) (* 10 2) (place-image DOT (* 10 7) (* 10 16) BG)))

(define (add-balloons p)
  (cond
    [(empty? p) BG]
    [else (place-image DOT (* 10 (posn-x (first p))) (* 10 (posn-y (first p)))
                       (add-balloons (rest p)))]))

; BallonList is one of:
; - empty
; - (cons Posn (cons BalloonList))

; BalloonList -> BalloonList
(define (riot bl)
  (big-bang bl
    [on-tick b-down 1]
    [to-draw b-draw]))


; BalloonList -> BalloonList
; decreasy posn-y of each balloon by 1
(check-expect (b-down (cons (make-posn 3 6) '())) (cons (make-posn 3 5) '()))
(check-expect (b-down (cons (make-posn 5 7) (cons (make-posn 2 9) '()))) (cons (make-posn 5 6) (cons (make-posn 2 8) '())))

(define (b-down b)
  (cond
    [(empty? b) '()]
    [else (cons (make-posn (posn-x (first b)) (- (posn-y (first b)) 1))
                (b-down (rest b)))]))


; BaloonList -> Image

(define (b-draw b) (add-balloons b))







;; Exercise 160.
;Design the functions set+.L and set+.R, which create a set by adding a number x to some given set s
;for the left-hand and right-hand data definition, respectively.

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

; Number Son.L -> Son.L
#;
(define (set+.L n set) ...)

(check-expect (set+.L 3 (cons 2 '())) (cons 3 (cons 2 '())))
(check-expect (set+.L 7 (cons 2 (cons 9 '()))) (cons 7 (cons 2 (cons 9 '()))))

(define (set+.L n set)
  (cond
    [(empty? set) '()]
    [else
     (cons n set)]))

; Number Son.R -> Son.R
#;
(define (set+.R n set) ...)

(check-expect (set+.R 3 (cons 2 '())) (cons 3 (cons 2 '())))
(check-expect (set+.R 7 (cons 2 (cons 9 '()))) (cons 7 (cons 2 (cons 9 '()))))
(check-expect (set+.R 7 (cons 7 (cons 9 '()))) (cons 7 (cons 9 '())))

(define (set+.R n set)
  (cond
    [(empty? set) '()]
    [(member? n set) set]
    [else
     (cons n set)]))

;; Exercise 161.
;Translate the examples into tests and make sure they all succeed.
;Then change the function in figure 64 so that everyone gets $14 per hour.
;Now revise the entire program so that changing the wage for everyone is a
;single change to the entire program and not several.

(define AMOUNT 14)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* AMOUNT h))

;; Exercise 162.
;No employee could possibly work more than 100 hours per week.
;To protect the company against fraud, the function should check that no item of the input
;list of wage* exceeds 100. If one of them does, the function should immediately signal an error.
;How do we have to change the function in figure 64 if we want to perform this basic reality check?
(check-expect (wage-checked (cons 4 (cons 5 '()))) (cons (wage 4) (cons (wage 5) '())))
;(check-expect (wage-checked (cons 144 '())) (error "employee has worked more than 100 hours"))

(define (wage-checked whrs)
  (cond
    [(empty? whrs) '()]
    [(> (first whrs) 100) (error "employee has worked more than 100 hours")]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))



;; Exercise 163.
;Design convertFC.
;The function converts a list of measurements in Fahrenheit to a list of Celsius measurements.

; List-of-measurements is one of:
; - Number
; - (cons Number lom)

#;
(define (convert lom) ...)

(check-within (convert (cons 32 '())) (cons 0 '()) 0.1)
(check-within (convert (cons 85 (cons 56 (cons 99 '())))) (cons 29.4 (cons 13.3 (cons 37.2 '()))) 0.1)

; lom -> lom
(define (convert lom)
  (cond
    [(empty? lom) '()]
    [else (cons (* (/ 5 9) (- (first lom) 32)) (convert (rest lom)))]))


;; Exercise 164.
;Design the function convert-euro, which converts a list of US$ amounts into a list of € amounts.
;Look up the current exchange rate on the web.

;Generalize convert-euro to the function convert-euro*, which consumes an exchange rate
;and a list of US$ amounts and converts the latter into a list of € amounts.

; lod -> lod
(define (convert-euro* lod rate)
  (cond
    [(empty? lod) '()]
    [else (cons (* rate (first lod)) (convert (rest lod)))]))

;; Exercise 165.
;Design the function subst-robot, which consumes a list of toy descriptions (one-word strings)
;and replaces all occurrences of "robot" with "r2d2"; all other descriptions remain the same.

;Generalize subst-robot to substitute. The latter consumes two strings, called new and old,
;and a list of strings. It produces a new list of strings by substituting all occurrences of old with new.

(define (is-robot? str) (string=? str "robot"))


(check-expect (subst-robot (cons "bike" (cons "doll" (cons "robot" '())))) (cons "bike" (cons "doll" (cons "r2d2" '()))))
(check-expect (subst-robot (cons "robot" '())) (cons "r2d2" '()))

; lot -> lot
(define (subst-robot lot)
  (cond
    [(empty? lot) '()]
    [(is-robot? (first lot)) (cons "r2d2" (subst-robot (rest lot)))]
    [else (cons (first lot) (subst-robot (rest lot)))]))


(check-expect (substitute (cons "apple" '()) "apple" "orange") (cons "orange" '()))
(check-expect (substitute (cons "bike" (cons "doll" (cons "robot" '()))) "doll" "ball") (cons "bike" (cons "ball" (cons "robot" '()))))

; los -> los
(define (substitute lot old new)
    (cond
    [(empty? lot) '()]
    [(string=? (first lot) old) (cons new (substitute (rest lot) old new))]
    [else (cons (first lot) (substitute (rest lot) old new))]))
