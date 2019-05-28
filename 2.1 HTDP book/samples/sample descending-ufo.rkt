;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |sample descending-ufo|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; A WorldState is a Number.
; interpretation number of pixels between the top and the UFO
 
(define WIDTH 300) ; distances in terms of pixels 
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay/offset (circle 10 "solid" "green") 0 15 (ellipse 30 20 "solid" "blue") ))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render]))
 
; WorldState -> WorldState
; computes next location of UFO 
(check-expect (nxt 11) 12)
(define (nxt y)
  (+ y 1))
 
; WorldState -> Image
; places UFO at given height into the center of MTSCN
(check-expect (render 11) (place-image (beside UFO (text "descending" 24 "green")) 150 11 MTSCN))
(define (render y)
  (place-image (beside UFO (text-ufo y)) (/ WIDTH 2) y MTSCN))


(define (text-ufo y)
  (cond [(< y CLOSE)
         (text "descending" 24 "green")]
        [(> y CLOSE)
         (text "approaching" 24 "green")]
        ((> y HEIGHT)
         (text "landed" 24 "green"))))


