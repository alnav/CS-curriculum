;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 31-51|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)
;; HTDP

;; Exercise 31
; Recall the letter program from Composing Functions.
; Here is how to launch the program and have it write its output to the interactions area:
; > (write-file
;    'stdout
;    (letter "Matthew" "Fisler" "Felleisen"))


; Dear Matthew,

 

; We have discovered that all people with the

; last name Fisler have won our lottery. So, 

; Matthew, hurry and pick up your prize.

 

; Sincerely, 


; Felleisen

; 'stdout

; Of course, programs are useful because you can launch them for many different inputs. Run letter on three inputs of your choice.
; Here is a letter-writing batch program that reads names from three files and writes a letter to one:
#;
(define (main in-fst in-lst in-signature out)
  (write-file out
              (letter (read-file in-fst)
                      (read-file in-lst)
                      (read-file in-signature))))
; The function consumes four strings: the first three are the names of input files and the last one serves as an output file.
; It uses the first three to read one string each from the three named files, hands these strings to letter,
; and eventually writes the result of this function call into the file named by out, the fourth argument to main.
; Create appropriate files, launch main, and check whether it delivers the expected letter in a given file.
#;
(define (letter fst lst signature-name)
  (string-append
   (opening fst)
   "\n\n"
   (body fst lst)
   "\n\n"
   (closing signature-name)))
#; 
(define (opening fst)
  (string-append "Dear " fst ","))
#; 
(define (body fst lst)
  (string-append
   "We have discovered that all people with the" "\n"
   "last name " lst " have won our lottery. So, " "\n"
   fst ", " "hurry and pick up your prize."))
#; 
(define (closing signature-name)
  (string-append
   "Sincerely,"
   "\n\n"
   signature-name
   "\n"))
#;
(define (main in-fst in-lst in-signature out)
  (write-file out
              (letter (read-file in-fst)
                      (read-file in-lst)
                      (read-file in-signature))))


;; Exercise 32
; Most people no longer use desktop computers just to run applications but also employ cell phones, tablets, and their cars’ information control screen.
; Soon people will use wearable computers in the form of intelligent glasses, clothes, and sports gear.
; In the somewhat more distant future, people may come with built-in bio computers that directly interact with body functions.
; Think of ten different forms of events that software applications on such computers will have to deal with. 

;; Exercise 33
; Research the “year 2000” problem.

;; Exercise 34
; Design the function string-first, which extracts the first character from a non-empty string. Don’t worry about empty strings.

;; String -> String
;; extract first letter of given string
(check-expect (string-first "ale") "a")

;(define (string-first str) "") ;stub

(define (string-first str) (substring str 0 1))

;; Exercise 35
; Design the function string-last, which extracts the last character from a non-empty string.

;; String -> String
;; extract last letter of given string
(check-expect (string-last "ale") "e")

;(define (string-first str) "") ;stub

(define (string-last str) (substring str (- (string-length str) 1) (string-length str)))

;; Exercise 36
; Design the function image-area, which counts the number of pixels in a given image.

;; Image -> Natural
;; returns area of image
(check-expect (image-area (square 10 "solid" "red")) 100)

;(define (image-area img) 0)
(define (image-area img)
  (* (image-width img)
     (image-height img)))

;; Exercise 37
; Design the function string-rest, which produces a string like the given one with the first character removed.
;; String -> String
;; returns a string without the first character
(check-expect (string-rest "ale") "le")

;(define (string-rest str) "")

(define (string-rest str) (substring str 1 (string-length str)))

;; Exercise 38
; Design the function string-remove-last, which produces a string like the given one with the last character removed.
;; String -> String
;; returns a string without the last character
(check-expect (string-remove-last "ale") "al")

;(define (string-remove-last str) "")

(define (string-remove-last str) (substring str 0 (- (string-length str) 1)))

;; Exercise 39
; Good programmers ensure that an image such as CAR can be enlarged or reduced via a single change to a constant definition.

; (define WHEEL-RADIUS 5)

; The definition of WHEEL-DISTANCE is based on the wheel’s radius.
; Hence, changing WHEEL-RADIUS from 5 to 10 doubles the size of the car image.
; This kind of program organization is dubbed single point of control, and good design employs this idea as much as possible.
; Develop your favorite image of an automobile so that WHEEL-RADIUS remains the single point of control.

(define WHEEL-RADIUS 10)

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define WHEEL-SPACE (rectangle (* 2.5 WHEEL-RADIUS) WHEEL-RADIUS "solid" "white"))
(define CAR-TOP (rectangle (* 2 WHEEL-RADIUS) WHEEL-RADIUS "solid" "black"))
(define CAR-BODY (rectangle (* 6 WHEEL-RADIUS) (* 1.5 WHEEL-RADIUS) "solid" "red"))

(define CAR (above CAR-TOP (overlay/offset  CAR-BODY 0 (/ WHEEL-RADIUS 1.2) (beside WHEEL WHEEL-SPACE WHEEL))))


;; Exercise 40
; Formulate the examples as BSL tests, that is, using the check-expect form. Introduce a mistake. Re-run the tests.

; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
;(define (tock ws) ws)

; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81

(define (tock2 ws)
  (+ ws 3))

(check-expect (tock2 3) 6)
(check-expect (tock2 9) 12)


;; Exercise 41
; Finish the sample problem and get the program to run.
; That is, assuming that you have solved exercise 39, define the constants BACKGROUND and Y-CAR.
; Then assemble all the function definitions, including their tests.
; When your program runs to your satisfaction, add a tree to the scenery.

; We used
(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

; to create a tree-like shape.
; Also add a clause to the big-bang expression that stops the animation when the car has disappeared on the right side.

;; Exercise 42
; Modify the interpretation of the sample data definition so that
; a state denotes the x-coordinate of the right-most edge of the car.

(define WIDTH 600)
(define HEIGHT 400)
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND (overlay/xy TREE -350 -350 (empty-scene WIDTH HEIGHT)))
(define Y-CAR (/ HEIGHT 2))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))

;; ws -> ws
;; Produces next y position of the car
(check-expect (tock 3) 6)
(check-expect (tock 9) 12)

(define (tock ws)
  (+ ws 3))


;; ws -> Image
;; Render world
(check-expect (render 50) (place-image CAR (- 50 (/ (image-width CAR) 2)) Y-CAR BACKGROUND))

(define (render ws)
  (place-image CAR (- ws (/ (image-width CAR) 2)) Y-CAR BACKGROUND))

(define (end? ws)
  (>= ws (image-width BACKGROUND)))

;; Exercise 43
; Let’s work through the same problem statement with a time-based data definition:
; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started
; Like the original data definition, this one also equates the states of the world with the class of numbers.
; Its interpretation, however, explains that the number means something entirely different.
; Design the functions tock and render.
; Then develop a big-bang expression so that once again you get an animation of a car traveling from left to right across the world’s canvas.

; How do you think this program relates to animate from Prologue: How to Program?

; Use the data definition to design a program that moves the car according to a sine wave. (Don’t try to drive like that.) 
(define SPEED 5)

(define (sine ws)
  (big-bang ws
    [on-tick tock-sin]
    [to-draw render-sin]
    [stop-when end-sin?]))

;; ws -> ws
;; Produces time since start (ws)
(check-expect (tock-sin 3) 4)
(check-expect (tock-sin 9) 10)

(define (tock-sin ws)
  (+ ws 1))

;; ws -> Natural
;; produces x position
(define (x-distance ws)
  (* ws SPEED))

;; ws -> Natural
;; produces y position
(define (y-distance ws)
  (- (/ (image-height BACKGROUND) 2)
     (sin ws)))

;; ws -> Image
;; Render world
(check-expect (render-sin 50) (place-image CAR (x-distance 50) (y-distance 50) BACKGROUND))

(define (render-sin ws)
  (place-image CAR (x-distance ws) (y-distance ws) BACKGROUND))

;; ws -> ws
;; stops if car touches right margin
(define (end-sin? ws)
  (>= (x-distance ws)
      (- (image-width BACKGROUND) (/ (image-width CAR) 2))))

;; Exercise 44
;Formulate the examples as BSL tests. Click RUN and watch them fail.

; .. Completed working function


(define (main2 ws)
  (big-bang ws
    [on-tick tock]
    [on-mouse hyper]
    [to-draw render]))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
(check-expect (hyper 10 5 30 "button-down") 5)

(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))



;; Exercise 45
;  Design a “virtual cat” world program that continuously moves the cat from left to right.
; Let’s call it cat-prog and let’s assume it consumes the starting position of the cat.
; Furthermore, make the cat move three pixels per clock tick. Whenever the cat disappears on the right, it reappears on the left.
; You may wish to read up on the modulo function.
(define cat1 (square 10 "solid" "red"))

;; ws -> ws
(define (cat ws)
  (big-bang ws
    [on-tick cat-tock]
    [to-draw cat-render]))

;; ws -> ws
;; Produces next y position of the cat
(check-expect (cat-tock 3) 6)
(check-expect (cat-tock 9) 12)

(define (cat-tock ws)
  (if (< ws WIDTH)
      (+ ws 3)
      0))

;; ws -> Image
;; draw cat
(check-expect (cat-render 50) (place-image cat1 50 Y-CAR BACKGROUND))

#;
(define (cat-render ws)
  (place-image cat1 ws Y-CAR BACKGROUND))


;; Exercise 46
; Improve the cat animation with a slightly different image:
; Adjust the rendering function from exercise 45 so that it uses one cat image or the other based on whether the x-coordinate is odd.
; Read up on odd? in the HelpDesk, and use a cond expression to select cat images.

(define cat2 (square 10 "solid" "red"))

(define (cat-render ws)
  (if (odd? ws)
      (place-image cat2 ws Y-CAR BACKGROUND)
      (place-image cat1 ws Y-CAR BACKGROUND)))


;; Exercise 47
; Design a world program that maintains and displays a “happiness gauge.”
; Let’s call it gauge-prog, and let’s agree that the program consumes the maximum level of happiness.
; The gauge display starts with the maximum score, and with each clock tick, happiness decreases by -0.1
; it never falls below 0, the minimum happiness score. Every time the down arrow key is pressed, happiness increases by 1/5,
; every time the up arrow is pressed, happiness jumps by 1/3.
; To show the level of happiness, we use a scene with a solid, red rectangle with a black frame.
; For a happiness level of 0, the red bar should be gone,
; for the maximum happiness level of 100, the bar should go all the way across the scene.
(define MAX-HAPPINESS 500)
(define GAUGE-WIDTH 500)
(define GAUGE-HEIGHT 200)
(define GAUGE-BACKGROUND (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black"))


;; ws -> ws
;; Gauge-prog world
(define (gauge-prog ws)
  (big-bang ws
    [on-tick gauge-tick]
    [on-key gauge-change]
    [to-draw gauge-render]))

;; ws -> ws
;; reduces happiness by 1 per tick
;; minimum happiness is 0
(check-expect (gauge-tick 10) 9)
(check-expect (gauge-tick 0) 0)

(define (gauge-tick ws)
  (cond [(= ws 0) 0]
        [(> ws MAX-HAPPINESS) MAX-HAPPINESS]
        [else
         (- ws 1)]))


;; ws KeyEvent -> ws
;; ws + 1/5 ws if down arrow pressed
;; ws + 1/3 ws if up arrow pressed
(check-expect (gauge-change 15 "down") 18)
(check-expect (gauge-change 15 "up") 20) 

(define (gauge-change ws ke)
  (cond [(key=? ke "down")
         (+ ws (* ws (/ 1 5)))]
        [(key=? ke "up")
         (+ ws (* ws (/ 1 3)))]
        [else
         ws]))

;; ws -> Image
;; Render gauge-prog
(check-expect (gauge-render 400)
              (overlay/align "left" "middle" (rectangle 400 GAUGE-HEIGHT "solid" "red") GAUGE-BACKGROUND))

(define (gauge-render ws)
  (overlay/align "left" "middle" (rectangle ws GAUGE-HEIGHT "solid" "red") GAUGE-BACKGROUND))


;; Exercise 48
; Enter the definition of reward followed by (reward 18) into the definitions area of DrRacket
; and use the stepper to find out how DrRacket evaluates applications of the function. 

(define (reward s)
  (cond
    [(<= 0 s 10) "bronze"]
    [(and (< 10 s) (<= s 20)) "silver"]
    [else "gold"]))

;; Exercise 49
; A cond expression is really just an expression and may therefore show up in the middle of another expression:
; (- 200 (cond [(> y 200) 0] [else y]))

; Use the stepper to evaluate the expression for y as 100 and 210.
    
;; Exercise 50
; If you copy and paste the above function definition into the definitions area of DrRacket and click RUN, DrRacket highlights two of the three cond lines.
; This coloring tells you that your test cases do not cover the full conditional. Add enough tests to make DrRacket happy.
; TrafficLight -> TrafficLight

; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

;; Exercise 51
; Design an animation of a traffic light. 

; Your program should show a traffic light that is red, then green, 
; then yellow, then red etc. For this program, your changing world 
; state data definition should be an enumeration.

;; Draw a traffic light, light changes every second

;; =================
;; Constants:
(define HEIGHTt 320)
(define WIDTHt 130)
(define RADIUS 50)
(define CIRCLE-RED-SOLID (circle RADIUS "solid" "red"))
(define CIRCLE-RED-OUTLINE (circle RADIUS "outline" "red"))
(define CIRCLE-YELLOW-SOLID (circle RADIUS "solid" "yellow"))
(define CIRCLE-YELLOW-OUTLINE (circle RADIUS "outline" "yellow"))
(define CIRCLE-GREEN-SOLID (circle RADIUS "solid" "green"))
(define CIRCLE-GREEN-OUTLINE (circle RADIUS "outline" "green"))
(define BOX (rectangle WIDTHt HEIGHTt "solid" "black"))
(define SCENE (empty-scene (* 2 WIDTHt) (* 2 HEIGHTt)))

(define LIGHT-RED(overlay (above CIRCLE-RED-SOLID CIRCLE-YELLOW-OUTLINE CIRCLE-GREEN-OUTLINE) BOX))
(define LIGHT-YELLOW(overlay (above CIRCLE-RED-OUTLINE CIRCLE-YELLOW-SOLID CIRCLE-GREEN-OUTLINE) BOX))
(define LIGHT-GREEN(overlay (above CIRCLE-RED-OUTLINE CIRCLE-YELLOW-OUTLINE CIRCLE-GREEN-SOLID) BOX))


;; =================
;; Data definitions:

;; color is Number
;; interp. color is what next color is active
;; 1 = RED
;; 2 = YELLOW
;; 3 = GREEN

(define color1 1) ;red
(define color2 2) ;yellow
(define color3 3) ;green

;(define (fn-for-color col) (cond [<question1> <answer1>]
;      [<question2> <answer2>]))


;; =================
;; Functions:

;; color -> color
;; start the world with color = 1
(define (mainT color)
  (big-bang color                ; color
            (on-tick   tockT 1)     ; color -> color
            (to-draw   renderT)))   ; color -> Image
            

;; color -> color
;; if color = 1 -> 2,
;; if color = 2 -> 3,
;; if color = 3 -> 1
(check-expect (tockT 1) 3)
(check-expect (tockT 3) 2)
(check-expect (tockT 2) 1)
    
(define (tockT color) (cond [(= color 1) 3]
                           [(= color 3) 2]
                           [(= color 2) 1]))
                                       


;; color -> Image
;; render image for traffic light depending on color 
(define (renderT color) (place-image (cond [(= color 1) LIGHT-RED]
                                          [(= color 2) LIGHT-YELLOW]
                                          [(= color 3) LIGHT-GREEN])
                                    (/ (image-width SCENE) 2) (/ (image-height SCENE) 2) SCENE))

