;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 52-57-nopic|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; HTDP

;; Exercise 52
;  Which integers are contained in the four intervals above? 

; [3,5] is a closed interval:        3, 4, 5
; (3,5] is a left-open interval:     4, 5
; [3,5) is a right-open interval:    3, 4
; and (3,5) is an open interval:     4

;; Exercise 53

; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in fligh
; The design recipe for world programs demands that you translate information
; into data and vice versa to ensure a complete understanding of the data definition.
; It’s best to draw some world scenarios and to represent them with data and, conversely,
; to pick some data examples and to draw pictures that match them.
; Do so for the LR definition, including at least HEIGHT and 0 as examples.
; LR = "resting"
; LR = HEIGHT
; LR = 0

;; Exercise 54
; Why would it be incorrect to use (string=? "resting" x) as the first condition in show?
; Conversely, formulate a completely accurate condition, that is,
; a Boolean expression that evaluates to #true precisely when x belongs to the first sub-class of LRCD.

(define (show x)
  (cond
    [(string? x) ...]
    [(<= -3 x -1) ...]
    [(>= x 0) ...]))

; ANSWER: because x could be a number, and checking string=? of a number would give an error message


;; Exercise 55
; Take another look at show. It contains three instances of an expression with the approximate shape:
; (place-image ROCKET 10 (- ... CENTER) BACKG)

; This expression appears three times in the function: twice to draw a resting rocket and once to draw a flying rocket.
; Define an auxiliary function that performs this work and thus shorten show.

;; Number -> Image
;; Auxiliary show function
; (define (show-rocket y) (place-image ROCKET 10 (- y CENTER) BACKG))



;; Exercise 56
; Define main2 so that you can launch the rocket and watch it lift off.
; Read up on the on-tick clause to determine the length of one tick and how to change it.

; If you watch the entire launch, you will notice that once the rocket reaches the top something curious happens.
; Explain.

; Add a stop-when clause to main2 so that the simulation of the liftoff stops gracefully when the rocket is out of sight.
(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 30)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (- HEIGHT (/ (image-height ROCKET) 2)))

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
    [to-draw show]
    [on-key launch]))


; LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect (show "resting")
              (show-rocket 0))
 
(check-expect (show -1)
              (place-image (text "-1" 20 "red")
                           10 (* 3/4 WIDTH)
                           (show-rocket 0)))
 
(check-expect (show 53)
              (show-rocket 53))

(define (show x)
  (cond
    [(string? x)
     (show-rocket 0)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (show-rocket 0))]
    [(>= x 0)
     (show-rocket x)]))


;; Number -> Image
;; Auxiliary show function
(define (show-rocket y) (place-image ROCKET 10 (- CENTER y) BACKG))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (+ x 1)]
    [(>= x 0) (+ x YDELTA)]))

;; LRCD -> Boolean
;; stop world when x < 0
(define (end? x)
  (and (number? x) (> x HEIGHT))) 

; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [on-tick fly 1]
    [to-draw show]
    [on-key launch]
    [stop-when end?]))

;; Exercise 57
; Recall that the word “height” forced us to choose one of two possible interpretations.
; Now that you have solved the exercises in this section, solve them again using the first interpretation of the word.
; Compare and contrast the solutions.

; .. Done above