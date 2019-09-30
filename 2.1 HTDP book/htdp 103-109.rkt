;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 103-109|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; Exercise 103.
;Develop a data representation for the following four kinds of zoo animals:

;- spiders, whose relevant attributes are the number of remaining legs
;  (we assume that spiders can lose legs in accidents) and the space they need in case of transport;
(define-struct spider [legs space])

;(make-spider [Natural Natural])

;- elephants, whose only attributes are the space they need in case of transport;
; Elephant is a Natural
; interp. space needed for transport
(define elephant1 100)

;- boa constrictors, whose attributes include length and girth;
(define-struct boa [length girth])

;(make-boa [Natural Natural])

;- armadillos, for which you must determine appropriate attributes, including one that determines the space needed for transport.
(define-struct armadillo [thickness space])

;(make-armadillo [Natural Natural])

;; Template for function that consumes zoo animals

(define (fun-animal a)
  [cond
    [(spider? a) (...)]
    [(number? a) (...)]
    [(boa? a) (...)]
    [(armadillo? a) (...)]])

; Design the fits? function, which consumes a zoo animal and a description of a cage.
; It determines whether the cage’s volume is large enough for the animal.

(define (fits? a cage)
  [cond
    [(spider? a) (< (spider-space a) cage)]
    [(number? a) (< a cage)]
    [(boa? a) (< (* (boa-length a) (boa-girth a)) cage)]
    [(armadillo? a) (< (armadillo-space a) cage)]])

;; Exercise 104.
;Your home town manages a fleet of vehicles: automobiles, vans, buses, and SUVs.
;Develop a data representation for vehicles.
;The representation of each vehicle must describe the number of passengers that it can carry,
;its license plate number, and its fuel consumption (miles per gallon).
;Develop a template for functions that consume vehicles.

; Vehicle is one of
; - automobile
; - van
; - bus
; - suv
; a vehicle has seats, plate, fuel consumption

(define-struct vehicle [type seats plate fuel])

; vehicle is one of:
; - (make-vehicle ["automobile" Natural String Natural])
; - (make-vehicle ["van" Natural String Natural])
; - (make-vehicle ["bus" Natural String Natural])
; - (make-vehicle ["suv" Natural String Natural])

(define (fun-vehicle v)
  [cond
    [(= (vehicle-type v) "automobile") (...)]
    [(= (vehicle-type v) "van") (...)]
    [(= (vehicle-type v) "bus") (...)]
    [(= (vehicle-type v) "suv") (...)]])

;; Exercise 105.
;Some program contains the following data definition:
; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point
;Make up at least two data examples per clause in the data definition. For each of the examples, explain its meaning with a sketch of a canvas.

; 30 on the y-axis
(define coord1 -30)
; 40 on the x-axis
(define coord2 40)
; cartesian point at 30 20
(define coord3 (make-posn 30 20))


;; Exercise 106.
;Design the cat-cham world program.
;Given both a location and an animal, it walks the latter across the canvas, starting from the given location.
;Here is the chosen data representation for animals:
; A VAnimal is either
; – a VCat
; – a VCham
;where VCat and VCham are your data definitions from exercises 88 and 92.
;Given that VAnimal is the collection of world states, you need to design
;a rendering function from VAnimal to Image;
;a function for handling clock ticks, from VAnimal to VAnimal; and
;a function for dealing with key events so that you can feed and pet and colorize your animal—as applicable.

(define WIDTH 640)
(define HEIGHT 480)
(define MTS (empty-scene WIDTH HEIGHT))
(define A-HEIGHT 240)

; A VAnimal is either
; – a VCat
; – a VCham

; A VCat is a (make-VCat [Number Number])
; interp. a cat with a x position and happiness level
(define-struct VCat [x happiness])

(define CAT (rectangle 100 30 "solid" "gray"))

; A VCat is a (make-VCham [Number Number])
; interp. a chameleon with a x position and hunger level
(define-struct VCham [x hunger])

(define CHAM (beside (rectangle 50 30 "solid" "green") (rectangle 50 30 "solid" "red")))

; VAnimal -> Image
; render VAnimal
; (define (Vrender va) (square 0 "solid" "white")) ;stub
(check-expect (Vrender (make-VCat 100 10)) (place-image CAT 100 A-HEIGHT MTS))
(check-expect (Vrender (make-VCham 150 10)) (place-image CHAM 150 A-HEIGHT MTS))

(define (Vrender va)
  [cond
    [(VCat? va)
     (place-image CAT (VCat-x va) A-HEIGHT MTS)]
    [(VCham? va)
     (place-image CHAM (VCham-x va) A-HEIGHT MTS)]])
    
; VAnimal -> VAnimal
; next clock tick
(check-expect (Vtock (make-VCat 100 40)) (make-VCat 105 38))
(check-expect (Vtock (make-VCham 100 40)) (make-VCham 105 38))

(define (Vtock va)
  [cond
    [(VCat? va)
     (make-VCat (+ (VCat-x va) 5) (- (VCat-happiness va) 2))]
    [(VCham? va)
     (make-VCham (+ (VCham-x va) 5) (- (VCham-hunger va) 2))]])

;; !!! TODO
;a function for dealing with key events so that you can feed and pet and colorize your animal—as applicable.


;; Exercise 107.
;Design the cham-and-cat program, which deals with both a virtual cat and a virtual chameleon.
;You need a data definition for a "zoo" containing both animals and functions for dealing with it.
;The problem statement leaves open how keys manipulate the two animals. Here are two possible interpretations:
;Each key event goes to both animals.
;Each key event applies to only one of the two animals.
;For this alternative, you need a data representation that specifies a focus animal,
;that is, the animal that can currently be manipulated.
;To switch focus, have the key-handling function interpret "k" for "kitty" and "l" for lizard.
;Once a player hits "k", the following keystrokes apply to the cat only—until the player hits "l".


;; Exercise 108.
;In its default state, a pedestrian crossing light shows an orange person standing on a red background.
;When it is time to allow the pedestrian to cross the street, the light receives a signal
;and switches to a green, walking person. This phase lasts for 10 seconds.
;After that the light displays the digits 9, 8, ..., 0 with odd numbers colored orange and even numbers colored green.
;When the countdown reaches 0, the light switches back to its default state.

;Design a world program that implements such a pedestrian traffic light.
;The light switches from its default state when you press the space bar on your keyboard.
;All other transitions must be reactions to clock ticks.
; default ... space -> green, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10 -> 9, 8, 7, 6, 5, 4, 3, 2, 1 
; default is 21, green is 20

(define ORANGE (rectangle 50 250 "solid" "orange"))
(define GREEN (rectangle 50 250 "solid" "green"))

(define-struct light [state image])

; a light is a structure
; (make-light [string string/number])
; interp:
; state is one of:
; - default
; - active

; image is Number[21,9]
; interp. image has 3 possible states, red figure(21), green figure(20), numbers from 19 to 10 (waiting), numbers from 9 to 1 (countdown
; odd numbers are colored orange, even number are colored green

(define (main l)
  (big-bang l
    [on-tick tock 1]
    [to-draw render]))

; light -> light
; interp. return next image if in active state, otherwise return default light
(check-expect (tock (make-light "default" 0)) (make-light "default" 0))
(check-expect (tock (make-light "active" 8)) (make-light "active" 7))
(check-expect (tock (make-light "active" 19)) (make-light "active" 18))
(check-expect (tock (make-light "active" 20)) (make-light "active" 19))
(check-expect (tock (make-light "active" 0)) (make-light "default" 0))

(define (tock l)
  [cond
    [(string=? "default" (light-state l))
     (make-light "default" 0)]
    [(> (light-image l) 0)
     (make-light "active" (- (light-image l) 1))]
    [(= 0 (light-image l))
     (make-light "default" 0)]])


; light -> image
; render image
(check-expect (render (make-light "default" 0)) ORANGE)
(check-expect (render (make-light "active" 20)) GREEN)
(check-expect (render (make-light "active" 15)) GREEN)
(check-expect (render (make-light "active" 6)) (textnum 6))

(define (render l)
  [cond
    [(string=? "default" (light-state l))
     ORANGE]
    [(and (< (light-image l) 21) (< 9 (light-image l)))
     GREEN]
    [else (textnum (light-image l))]])



; number -> image
; return image of number, green if even, orange if odd
(check-expect (textnum 10) (text "10" 48 "green"))
(check-expect (textnum 9) (text "9" 48 "orange "))
(define (textnum n)
  (text (number->string n) 48
        (if (= (modulo n 2) 0) "green" "orange")))


; Exercise 109. Design a world program that recognizes a pattern in a sequence of KeyEvents.
;Initially the program shows a 100 by 100 white rectangle.
;Once your program has encountered the first desired letter, it displays a yellow rectangle of the same size.
;After encountering the final letter, the color of the rectangle turns green.
;If any "bad" key event occurs, the program displays a red rectangle.
;The specific sequences that your program looks for start with "a",
;followed by an arbitrarily long mix of "b" and "c", and ended by a "d".
;Clearly, "acbd" is one example of an acceptable string; two others are "ad" and "abcbbbcd".
;Of course, "da", "aa", or "d" do not match.

 

(define RECT (rectangle 100 100 "solid" "white"))
(define RECT-y (rectangle 100 100 "solid" "yellow"))
(define RECT-g (rectangle 100 100 "solid" "green"))
(define RECT-r (rectangle 100 100 "solid" "red"))

; state is one of:
; - AA
; - BB
; - DD
; - ER
;interp.
; AA is initial state, if input is "a", moves onto state BB, any other input moves to state ER
; BB is expect state, if input is "b" or "c", stays in BB state, if input is "d" moves to finished state, any other input moves to state ER
; DD if finished state, game is over
; ER is error state, game is over

 

(define (pattern s)
  (big-bang s
    [on-key key-pressed]
    [on-draw render-pattern]))

 

; state -> state
#;
(define (key-pressed s k) (...))
(check-expect (key-pressed "AA" "a") "BB")
(check-expect (key-pressed "AA" "c") "ER")
(check-expect (key-pressed "BB" "b") "BB")
(check-expect (key-pressed "BB" "c") "BB")
(check-expect (key-pressed "BB" "d") "DD")
(check-expect (key-pressed "BB" "x") "ER")


(define (key-pressed s k)
[cond
  [(string=? s "AA")
   [cond
     [(key=? k "a") "BB"]
     [else "ER"]]]
  [(string=? s "BB")
   [cond
     [(key=? k "d") "DD"]
     [(or (key=? k "b") (key=? k "c")) "BB"]
     [else "ER"]]]])

 

; state -> image
; interp. Render square depending of current state
(check-expect (render-pattern "AA") RECT)
(check-expect (render-pattern "BB") RECT-y)
(check-expect (render-pattern "DD") RECT-g)
(check-expect (render-pattern "ER") RECT-r)

 
(define (render-pattern s)
[cond
  [(string=? s "AA") RECT]
  [(string=? s "BB") RECT-y]
  [(string=? s "DD") RECT-g]
  [(string=? s "ER") RECT-r]])

 










