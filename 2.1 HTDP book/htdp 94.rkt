;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 94|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Exercise 94.
;Draw some sketches of what the game scenery looks like at various stages.
;Use the sketches to determine the constant and the variable pieces of the game.
;For the former, develop physical and graphical constants that describe the dimensions of the world (canvas) and its objects.
;Also develop some background scenery.
;Finally, create your initial scene from the constants for the tank, the UFO, and the background.

(define WIDTH 640)
(define HEIGHT 480)

(define MTS (empty-scene WIDTH HEIGHT))

(define UFO (above (circle 6 "solid" "green") (ellipse 30 15 "solid" "green")))
(define TANK (above (rectangle 10 15 "solid" "black") (rectangle 30 10 "solid" "red")))
(define PROJECTILE (ellipse 8 20 "solid" "blue"))

(define-struct ufo [x y])
; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

(define-struct missile [x y])
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game


(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])


;; Exercise 95.
;Explain why the three instances are generated according to the first or second clause of the data definition. 
;Here is an instance that describes the tank maneuvering into position to fire the missile:
;(make-aim (make-posn 20 10) (make-tank 28 -3))
;
;This one is just like the previous one but the missile has been fired:
;(make-fired (make-posn 20 10)
;            (make-tank 28 -3)
;            (make-posn 28 (- HEIGHT TANK-HEIGHT)))
;Of course, the capitalized names refer to the physical constants that you defined.
;Finally, here is one where the missile is about to collide with the UFO:
;(make-fired (make-posn 20 100)
;            (make-tank 100 3)
;            (make-posn 22 103))


;; Exercise 96.
;Sketch how each of the three game states could be rendered assuming a image canvas.
;1. UFO at 20 10, tank at 28, going left
;2. UFO at 20 10, tank at 28, going left, just shot, with missile at 28 and tip of tank
;3. UFO at 20 100, tank at 100 going right, missile at 22 103, colliding with UFO

;; Exercise 97.
;Design the functions tank-render, ufo-render, and missile-render.

; SIGS -> Image
; renders the given game state on top of BACKGROUND 
; for examples see figure 32
#;
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   BACKGROUND)))]))

