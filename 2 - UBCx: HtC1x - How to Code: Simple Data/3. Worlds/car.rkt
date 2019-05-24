;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)

(define WHEELSIZE 10)
(define WHEEL (circle WHEELSIZE "solid" "black"))
(define RECT (rectangle (* WHEELSIZE 4) (* WHEELSIZE 1.5) "solid" "blue"))
(define TREE
  (underlay/xy (circle 50 "solid" "green")
               45 75
               (rectangle 10 100 "solid" "brown")))
(define CAR (beside WHEEL RECT WHEEL))
 (define BACKGROUND (overlay/xy TREE -350 -350 (empty-scene 600 600)))
(define Y-CAR (/ (image-height BACKGROUND) 2))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     [stop-when end?]))

; Number -> Number
; Increase tick by 3
(check-expect (tock 4) 14)

(define (tock ws)
  (+ ws 10))

; WorldState -> Image
; Place car into background according to worldstate
(check-expect (render 50) (place-image CAR 5 Y-CAR BACKGROUND))

(define (render ws)
  (place-image CAR (- ws 45) Y-CAR BACKGROUND))

(define (end? ws)
  (>= ws (image-width BACKGROUND)))

(main 0)

