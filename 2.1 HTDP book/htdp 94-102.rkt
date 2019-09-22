;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 94-102|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define TANK-HEIGHT 400)
(define MISSILE (ellipse 8 20 "solid" "blue"))


; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

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
; adds TANK, UFO, and possibly MISSILE to 
; the BACKGROUND scene
#;
(define (si-render s) BACKGROUND)

#;
(define (si-render s)
  (cond
    [(aim? s) (... (aim-tank s) ... (aim-ufo s) ...)]
    [(fired? s) (...(fired-tank s) ... (fired-ufo s) ... (fired-missile s) ...)]))

; Tank Image -> Image 
; adds t to the given image im
(check-expect (tank-render (make-tank 30 2) MTS) (place-image TANK 30 TANK-HEIGHT MTS))

(define (tank-render t im)
  (place-image TANK (tank-loc t) TANK-HEIGHT im))
 
; UFO Image -> Image 
; adds u to the given image im
(check-expect (ufo-render (make-posn 100 150) MTS) (place-image UFO 100 150 MTS))

(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image 
; adds m to the given image im
(check-expect (missile-render (make-posn 100 150) MTS) (place-image MISSILE 100 150 MTS))

(define (missile-render u im)
  (place-image MISSILE (posn-x u) (posn-y u) im))


; SIGS -> Image
; renders the given game state on top of BACKGROUND 
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) MTS))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   MTS)))]))

;; Exercise 98
; Design the function si-game-over? for use as the stop-when handler.
; The game stops if the UFO lands or if the missile hits the UFO.
; For both conditions, we recommend that you check for proximity of one object to another.

;The stop-when clause allows for an optional second sub-expression,
;namely a function that renders the final state of the game.
;Design si-render-final and use it as the second part
;for your stop-when clause in the main function of exercise 100.


; SIGS -> Boolean
; Stop game when UFO lands, or missile hits UFO
(check-expect (si-game-over? (make-aim (make-posn 100 475) (make-tank 3 10))) #true)
(check-expect (si-game-over? (make-aim (make-posn 100 100) (make-tank 3 10))) #false)
(check-expect (si-game-over? (make-fired (make-posn 100 100) (make-tank 3 10) (make-posn 100 100))) #true)
(check-expect (si-game-over? (make-fired (make-posn 100 100) (make-tank 3 10) (make-posn 50 100))) #false)

(define (si-game-over? s)
  (cond
    [(aim? s) (ufo-landed? s)]
    [(fired? s) (or (ufo-landed? s) (missile-hit? s))]))


; Image -> Image
; Final render after game over
(define (si-render-final s) (place-image (text "GAME OVER!" 50 "red") 320 240 MTS))


; SIGS -> Boolean
; return true if (- HEIGHT ufo-y) is less than 10
; interp. UFO has landed
(check-expect (ufo-landed? (make-aim (make-posn 100 475) (make-tank 3 10))) #true)
(check-expect (ufo-landed? (make-aim (make-posn 100 425) (make-tank 3 10))) #false)

(define (ufo-landed? u)
  [cond
    [(aim? u) (if (< (- HEIGHT (posn-y (aim-ufo u))) 10) #true #false)]
    [(fired? u) (if (< (- HEIGHT (posn-y (fired-ufo u))) 10) #true #false)]])

; SIGS -> Boolean
; return true if missile has hit UFO

(check-expect (missile-hit? (make-fired (make-posn 100 100) (make-tank 3 10) (make-posn 100 100))) #true)
(check-expect (missile-hit? (make-fired (make-posn 100 100) (make-tank 3 10) (make-posn 50 100))) #false)
(check-expect (missile-hit? (make-fired (make-posn 100 100) (make-tank 3 10) (make-posn 0 0))) #false)

(define (missile-hit? m)
  (if (and (< (abs (- (posn-y (fired-missile m)) (posn-y (fired-ufo m)))) 10)
           (< (abs (- (posn-x (fired-missile m)) (posn-x (fired-ufo m)))) 10)) #true #false))


;; Exercise 99.
;Design si-move.
;This function is called for every clock tick to determine to which position the objects move now.
;Accordingly, it consumes an element of SIGS and produces another one.

; SIGS -> SIGS
; moves elements of game.
; ufo moves down, and jumps randomly to left and right
; tank moves +vel fired (make-fired (make-posn (posn-x (fired-ufo s)) (+ (posn-y (fired-ufo s)) 5))
; missile moves up  (make-aim (make-posn (posn-x (aim-ufo s)) (+ (posn-y (aim-ufo s)) 5))
(check-expect (si-move (make-aim (make-posn 100 100) (make-tank 100 5)))
              (make-aim (make-posn 100 105) (make-tank 105 5)))
(check-expect (si-move (make-fired (make-posn 100 100) (make-tank 100 5) (make-posn 100 50)))
                       (make-fired (make-posn 100 105) (make-tank 105 5) (make-posn 100 40)))

(define (si-move s)
  [cond
    [(aim? s) (make-aim (make-posn (posn-x (aim-ufo s)) (+ (posn-y (aim-ufo s)) 5))
                        (make-tank (+ (tank-loc (aim-tank s)) (tank-vel (aim-tank s))) (tank-vel (aim-tank s))))]
    [(fired? s) (make-fired (make-posn (posn-x (fired-ufo s)) (+ (posn-y (fired-ufo s)) 5))
                            (make-tank (+ (tank-loc (fired-tank s)) (tank-vel (fired-tank s))) (tank-vel (fired-tank s)))
                            (make-posn (posn-x (fired-missile s)) (- (posn-y (fired-missile s)) 10)))]])

;!!! TODO - add to si-move
; Number -> Number
; produces a number in the interval [0,n),
; possibly a different one each time it is called 
(define random-ufo (random WIDTH))

;; Exercise 100.
;Design the function si-control, which plays the role of the key-event handler.
;As such, it consumes a game state and a KeyEvent and produces a new game state.

;It reacts to three different keys:
;pressing the left arrow ensures that the tank moves left;
;pressing the right arrow ensures that the tank moves right; and
;pressing the space bar fires the missile if it hasn’t been launched yet.

; SIGS KeyEvent -> SIGS
;; modified game state based on key pressed
(check-expect (si-control (make-aim (make-posn 100 50) (make-tank 100 5)) " ")
              (make-fired (make-posn 100 50) (make-tank 100 5) (make-posn 100 TANK-HEIGHT)))

(define (si-control s ke)
  (cond
    [(aim? s)
     (cond
       [(string=? "left" ke) (make-aim (aim-ufo s) (make-tank (tank-loc (aim-tank s)) -5))]
       [(string=? "right" ke) (make-aim (aim-ufo s) (make-tank (tank-loc (aim-tank s)) 5))]
       [(string=? " " ke) (make-fired (aim-ufo s) (make-tank (tank-loc (aim-tank s)) 5) (make-posn (tank-loc (aim-tank s)) TANK-HEIGHT))])]
    [(fired? s) (cond
       [(string=? "left" ke) (make-fired (fired-ufo s) (make-tank (tank-loc (fired-tank s)) -5) (fired-missile s))]
       [(string=? "right" ke) (make-aim (fired-ufo s) (make-tank (tank-loc (fired-tank s)) 5) (fired-missile s))]
       [(string=? " " ke) s])]))
   

;; Exercise 101.
;Turn the examples in figure 35 into test cases.

; Exercise 102.
;Design all other functions that are needed to complete the game for this second data definition.
;; SIGS -> SIGS
(define (main sigs)
  (big-bang sigs
    [on-tick si-move]
    [to-draw si-render]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]))

(main (make-aim (make-posn 100 10) (make-tank 80 5)))