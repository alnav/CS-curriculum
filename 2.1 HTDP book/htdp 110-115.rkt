;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 110-115|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 110.
;A checked version of area-of-disk can also enforce that the arguments
;to the function are positive numbers, not just arbitrary numbers.
;Modify checked-area-of-disk in this way.

; Number -> Number
; computes area of disk with radius v
(define (area-of-disk v)
  (* 3.14 (* v v)))

; Any -> Number
; computes the area of a disk with radius v, 
; if v is a number
(define (checked-area-of-disk v)
  (cond
    [(and (> v 0) (number? v)) (area-of-disk v)]
    [else (error "area-of-disk: positive number expected")]))

;; Exercise 111.
;Take a look at these definitions:
(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

;Develop the function checked-make-vec, which is to be understood as a checked version of the primitive operation make-vec.
;It ensures that the arguments to make-vec are positive numbers. In other words, checked-make-vec enforces our informal data definition.
(define (checked-make-vec x y)
  (cond
    [(and (> x 0) (> y 0))
     (make-vec x y)]
    [else
     (error "checked-make-vec: x and y must be positive numbers")]))


;; Exercise 112.
;Reformulate the predicate now using an or expression. 
(define (missile-or-not? v)
  (cond
    [(or (false? v) (posn? v)) #true]
    [else #false]))


;; Exercise 113.
;Design predicates for the following data definitions from the preceding section:
;SIGS, Coordinate (exercise 105), and VAnimal.

;; Exercise 114.
;Use the predicates from exercise 113 to check the space invader world program,
;the virtual pet program (exercise 106), and the editor program (A Graphical Editor).

;; Exercise 115.
;Revise light=? so that the error message specifies which of the two arguments isn’t an element of TrafficLight.