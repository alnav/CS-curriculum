;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 116-128|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 116.
;Take a look at the following sentences:
; x
; (= y z)
; (= (= y z) 0)
;Explain why they are syntactically legal expressions

;; Exercise 117.
;Consider the following sentences:
;(3 + 4)
;number?
;(x)
;Explain why they are syntactically illegal.

;; Exercise 118.
;Take a look at the following sentences:
;(define (f x) x)
;(define (f x) y)
;(define (f x y) 3)
;Explain why they are syntactically legal definitions

;; Exercise 119.
;Consider the following sentences:
;(define (f "x") x)
;(define (f x y z) (x))
;Explain why they are syntactically illegal. 

;; Exercise 120.
;Discriminate the legal from the illegal sentences:
;(x)
;(+ 1 (not x))
;(+ 1 2 3)
;Explain why the sentences are legal or illegal. Determine whether the legal ones belong to the category expr or def.

;; Exercise 121.
;Evaluate the following expressions step-by-step:
(+ (* (/ 12 8) 2/3)
   (- 20 (sqrt 4)))

(cond
  [(= 0 0) #false]
  [(> 0 1) (string=? "a" "a")]
  [else (= (/  1 0) 9)])

(cond	  [(= 2 0) #false]
          [(> 2 1) (string=? "a" "a")]
          [else (= (/  1 2) 9)])
;Use DrRacket’s stepper to confirm your computations.

;; Exercise 122.
;Suppose the program contains these definitions:

(define (f x y)
  (+ (* 3 x) (* y y)))

;Show how DrRacket evaluates the following expressions, step-by-step:
;(+ (f 1 2) (f 2 1))
;(f 1 (* 2 3))
;(f (f 1 (* 2 3)) 19)
;Use DrRacket’s stepper to confirm your computations

;; Exercise 123.
;The use of if may have surprised you in another way because this intermezzo does not mention this form elsewhere.
;In short, the intermezzo appears to explain and with a form that has no explanation either.
;At this point, we are relying on your intuitive understanding of if as a short-hand for cond. Write down a rule that shows how to reformulate
;(if exp-test exp-then exp-else)
;as a cond expression.

;; Exercise 124.
;Evaluate the following program, step-by-step:
(define PRICE 5)
(define SALES-TAX (* 0.08 PRICE))
(define TOTAL (+ PRICE SALES-TAX))

;Does the evaluation of the following program signal an error?
(define COLD-F 32)
(define COLD-C (fahrenheit->celsius COLD-F))
(define (fahrenheit->celsius f)
 (* 5/9 (- f 32)))

;How about the next one?
(define LEFT -100)
(define RIGHT 100)
(define (f x) (+ (* 5 (expt x 2)) 10))
(define f@LEFT (f LEFT))
(define f@RIGHT (f RIGHT))

;; Exercise 125.
;Discriminate the legal from the illegal sentences:
;(define-struct oops [])
;(define-struct child [parents dob date])
;(define-struct (child person) [dob date])
;Explain why the sentences are legal or illegal.

;; Exercise 126.
;Identify the values among the following expressions, assuming the definitions area contains these structure type definitions:
(define-struct point [x y z])
(define-struct none  [])
(make-point 1 2 3)
(make-point (make-point 1 2 3) 4 5)
(make-point (+ 1 2) 3 4)
(make-none)
(make-point (point-x (make-point 1 2 3)) 4 5)
Explain why the expressions are values or not. 

;; Exercise 127.
;Suppose the program contains
(define-struct ball [x y speed-x speed-y])

;Predict the results of evaluating the following expression:
(number? (make-ball 1 2 3 4))
(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3))
(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3))
(ball-x (make-posn 1 2))
(ball-speed-y 5)

;Check your predictions in the interactions area and with the stepper.

;; Exercise 128.
;Copy the following tests into DrRacket’s definitions area:
(check-member-of "green" "red" "yellow" "grey")
(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)  0.01)
(check-range #i0.9 #i0.6 #i0.8)
(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))
(check-satisfied 4 odd?)
;Validate that all of them fail and explain why. 