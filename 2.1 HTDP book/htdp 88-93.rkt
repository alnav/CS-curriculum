;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 88-93|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;; Exercise 88.
;Define a structure type that keeps track of the cat’s x-coordinate and its happiness.
;Then formulate a data definition for cats, dubbed VCat, including an interpretation. 
#;
(define-struct cat (xpos happiness))

; a cat is a structure:
; (make-cat Natural Natural)
; interp. (make-cat xpos happiness)
; cat with a x position and happiness level


;;Exercise 89.
;Design the happy-cat world program, which manages a walking cat and its happiness level.
;Let’s assume that the cat starts out with perfect happiness.

;;Exercise 90.
;Modify the happy-cat program from the preceding exercises so that it stops whenever the cat’s happiness falls to 0. 

;;Exercise 91.
;Extend your structure type definition and data definition from exercise 88 to include a direction field.
;Adjust your happy-cat program so that the cat moves in the specified direction.
;The program should move the cat in the current direction, and it should turn the cat around when it reaches either end of the scene.



(define-struct cat (x happiness))

(define MAX-HAPPINESS 600)
(define GAUGE-WIDTH 600)
(define GAUGE-HEIGHT 50)
(define GAUGE-BACKGROUND (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black"))

(define CATPIC (square 10 "solid" "red"))
(define WIDTH 600)
(define HEIGHT 400)
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND (overlay/xy TREE -350 -350 (empty-scene WIDTH HEIGHT)))
(define Y-CAR (/ HEIGHT 2))

;; ws -> ws
(define (catworld ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]))

;; ws -> ws
;; Produces next y position of the cat
(check-expect (tock (make-cat 3 400)) (make-cat 8 395))
(check-expect (tock (make-cat 9 100)) (make-cat 14 95))

(define (tock ws)
  (if (< (cat-happiness ws) 5)
      (make-cat (cat-x ws) 0)
      (if (< (cat-x ws) WIDTH)
      (make-cat (+ (cat-x ws) 5) (- (cat-happiness ws) 5))
      (make-cat 0 (cat-happiness ws)))))

;; ws -> Image
;; draw cat
(check-expect (render (make-cat 50 50)) (above (overlay/align "left" "middle" (rectangle 50 GAUGE-HEIGHT "solid" "red") GAUGE-BACKGROUND) (place-image CATPIC 50 Y-CAR BACKGROUND)))

(define (render ws)
  (above (overlay/align "left" "middle" (rectangle (cat-happiness ws) GAUGE-HEIGHT "solid" "red") GAUGE-BACKGROUND) (place-image CATPIC (cat-x ws) Y-CAR BACKGROUND)))


;; Exercise 92.
;Design the cham program, which has the chameleon continuously walking across the canvas from left to right.
;When it reaches the right end of the canvas, it disappears and immediately reappears on the left.
;Like the cat, the chameleon gets hungry from all the walking, and, as time passes by, this hunger expresses itself as unhappiness.

;; Exercise 93.
;Copy your solution to exercise 92 and modify the copy so that the chameleon walks across a tricolor background.


(define CHAMPIC (square 10 "solid" "blue")) 
(define BACKGROUNDCH
  (beside (empty-scene WIDTH HEIGHT "green")
          (empty-scene WIDTH HEIGHT "white")
          (empty-scene WIDTH HEIGHT "red")))


(define-struct cham (x hunger))

;; ws -> ws
(define (chamworld ws)
  (big-bang ws
    [on-tick tock-cham]
    [to-draw render-cham]))

;; ws -> ws
;; Produces next y position of the cat
(check-expect (tock-cham (make-cham 3 400)) (make-cham 8 395))
(check-expect (tock-cham (make-cham 9 100)) (make-cham 14 95))

(define (tock-cham ws)
  (if (< (cham-hunger ws) 5)
      (make-cham (cham-x ws) 0)
      (if (< (cham-x ws) (image-width BACKGROUNDCH))
      (make-cham (+ (cham-x ws) 5) (- (cham-hunger ws) 5))
      (make-cham 0 (cham-hunger ws)))))

;; ws -> Image
;; draw cat
(check-expect (render-cham (make-cham 50 50)) (above (overlay/align "left" "middle" (rectangle 50 GAUGE-HEIGHT "solid" "red") GAUGE-BACKGROUND) (place-image CHAMPIC 50 Y-CAR BACKGROUNDCH)))

(define (render-cham ws)
  (above (overlay/align "left" "middle" (rectangle (cham-hunger ws) GAUGE-HEIGHT "solid" "red") GAUGE-BACKGROUND) (place-image CHAMPIC (cham-x ws) Y-CAR BACKGROUNDCH)))


