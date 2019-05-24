;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)


(define BACKGROUND (empty-scene 600 600))

(define (main ws)
(big-bang ws
  (on-tick tock)
  (to-draw render)
  (stop-when end?)))


(define (tock ws) (- ws 1))
(define (render ws) (place-image (text (number->string (round ws)) 50 "blue") 300 300 BACKGROUND))
(define (end? ws) (= ws 0))

(main 10)