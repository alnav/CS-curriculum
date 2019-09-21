;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 11-20|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; HTDP

;; Exercise 11
; Define a function that consumes two numbers, x and y, and that computes the distance of point (x,y) to the origin.

;; Number Number -> Number
;; takes 2 numbers (x, y), produces distance of point (x, y) from the origin
(check-expect (distance 3 4) 5)

(define (distance x y) (sqrt(+ (sqr x) (sqr y))))


;; Exercise 12
; Define the function cvolume, which accepts the length of a side of an equilateral cube and computes its volume.
; If you have time, consider defining csurface, too.

;; Number -> Number
;; Takes length of side of a cube, produces volume
(check-expect (cvolume 3) 27)

(define (cvolume length) (* length length length))

;; Number -> Number
;; Takes length of side of a cube, produces surface area
(check-expect (csurface 3) 54)

(define (csurface length) (* (* length length) 6))

  
;; Exercise 13
; Define the function string-first, which extracts the first 1String from a non-empty string.

;; String -> String
;; Produces first 1String of String
(check-expect (string-first "Ale") "A")

(define (string-first str) (substring str 0 1))

;; Exercise 14
; Define the function string-last, which extracts the last 1String from a non-empty string.

;; String -> String
;; Produces last 1String of String
(check-expect (string-last "Ale") "e")

(define (string-last str) (substring str (- (string-length str) 1) (string-length str)))



;; Exercise 15
; Define ==>. The function consumes two Boolean values, call them sunny and friday.
; Its answer is #true if sunny is false or friday is true.
; Note Logicians call this Boolean operation implication, and they use the notation sunny => friday for this purpose.

;; Boolean Boolean -> Boolean
;; definition of ==>
(check-expect (==> #true #true) #true)
(check-expect (==> #false #false) #true)
(check-expect (==> #true #false) #false)

(define (==> b1 b2)
  (or (not b1) b2))

;; Exercise 16
; Define the function image-area, which counts the number of pixels in a given image.

;; Image -> Natural
;; Produces area of image
(check-expect (image-area (square 10 "solid" "red")) 100)
(check-expect (image-area (rectangle 10 20 "solid" "red")) 200)

(define (image-area img)
  (* (image-width img)
     (image-height img)))



;; Exercise 17
; Define the function image-classify, which consumes an image and conditionally produces
; "tall" if the image is taller than wide, "wide" if it is wider than tall, or "square" if its width and height are the same.

;; Image -> String
;; Produces "tall", "wide" or "square" based on image dimensions
(check-expect (image-classify (square 10 "solid" "red")) "square")
(check-expect (image-classify (rectangle 10 20 "solid" "red")) "tall")
(check-expect (image-classify (rectangle 30 20 "solid" "red")) "wide")

(define (image-classify img)
  (cond [(> (image-height img)
            (image-width img))
         "tall"]
        [(< (image-height img)
            (image-width img))
         "wide"]
        [else
         "square"]))

;; Exercise 18
; Define the function string-join, which consumes two strings and appends them with "_" in between.

;; String String -> String
;; Joins 2 strings, with "_" in between
(check-expect (string-join "ale" "best") "ale_best")

(define (string-join s1 s2) (string-append s1 "_" s2))

;; Exercise 19
; Define the function string-insert, which consumes a string str plus a number i and inserts "_" at the ith position of str.
; Assume i is a number between 0 and the length of the given string (inclusive)

;; String Natural[0, (string-length String)] -> String
;; Joins 2 strings, with "_" in between
(check-expect (string-insert "alessio" 2) "al_essio")

(define (string-insert str n) (string-append (substring str 0 n) "_" (substring str n (string-length str))))


;; Exercise 20
; Define the function string-delete, which consumes a string plus a number i and deletes the ith position from str.
; Assume i is a number between 0 (inclusive) and the length of the given string (exclusive)

;; String Natural[0, (string-length String)] -> String
;; Joins 2 strings, with "_" in between
(check-expect (string-delete "alessio" 2) "alssio")

(define (string-delete str n) (string-append (substring str 0 n) (substring str (+ n 1) (string-length str))))
