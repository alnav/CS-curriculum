;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 21-30|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/image)
;; HTDP

;; Exercise 21
; Use DrRacket’s stepper to evaluate (ff (ff 1)) step-by-step. Also try (+ (ff 1) (ff 1)).
; Does DrRacket’s stepper reuse the results of computations?

; ANSWER: No, DrRacket's stepper calculate every expression individually
;(define (ff x) (* 10 x))
;(ff (ff 1))
;(+ (ff 1) (ff 1))


;; Exercise 22
; Use DrRacket’s stepper on this program fragment:
; (define (distance-to-origin x y)
;   (sqrt (+ (sqr x) (sqr y))))
; (distance-to-origin 3 4)


(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

(distance-to-origin 3 4)

;; Exercise 23
; The first 1String in "hello world" is "h". How does the following function compute this result?
; (define (string-first s)
;   (substring s 0 1))
; Use the stepper to confirm your ideas.


(define (string-first s)
  (substring s 0 1))

(string-first "hello")

;; Exercise 24
; Here is the definition of ==>: y
; (define (==> x y)
;   (or (not x) y))
; Use the stepper to determine the value of (==> #true #false)


(define (==> x y)
  (or (not x) y))

(==> #true #false)

;; Exercise 25
; Take a look at this attempt to solve exercise 17:
 (define (image-classify img)
   (cond
     [(>= (image-height img) (image-width img)) "tall"]
     [(= (image-height img) (image-width img)) "square"]
     [(<= (image-height img) (image-width img)) "wide"]))

; Does stepping through an application suggest a fix?

; ANSWER: first and last clause should not have =
;         also, last clause can be an else

;; Exercise 26
; What do you expect as the value of this program:
 (define (string-insert s i)
  (string-append (substring s 0 i)
                 "_"
                 (substring s i)))
 
(string-insert "helloworld" 6)
; Confirm your expectation with DrRacket and its stepper

; ANSWER: "hellow_orld"

;; Exercise 27
; Our solution to the sample problem contains several constants in the middle of functions.
; As One Program, Many Definitions already points out, it is best to give names to such constants
; so that future readers understand where these numbers come from.
; Collect all definitions in DrRacket’s definitions area and change them
; so that all magic numbers are refactored into constant definitions.

(define FIXED-TICKET 120)
(define TICKET-BASE 5.0)
(define TICKET-MULTIPLIER (/ 15 0.1))
(define FIXED-COST 180)
;(define VARIABLE-COST 0.04)
 (define VARIABLE-COST 1.5) 

; number of attendees depends on ticket-price
(define (attendees ticket-price)
  (- FIXED-TICKET (* (- ticket-price TICKET-BASE) TICKET-MULTIPLIER)))

; revenue is exactly the product of ticket price and number of attendees
(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

; cost has 2 parts to it, a fixed part, and a part determined by number of attendees
#;
(define (cost ticket-price) ;old function
  (+ FIXED-COST (* VARIABLE-COST (attendees ticket-price))))

(define (cost ticket-price)
  (* VARIABLE-COST (attendees ticket-price)))
; profit difference between revenue and costs for some given ticket price
(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

;; Exercise 28
; Determine the potential profit for these ticket prices: $1, $2, $3, $4, and $5.
; Which price maximizes the profit of the movie theater? Determine the best ticket price to a dime.

;(profit 1.0)  ;511.2
;(profit 2.0)  ;937.2
;(profit 3.0)  ;1063.2
;(profit 4.0)  ;889.2
;(profit 5.0)  ;415.2


;; Exercise 29
; Exercise 29. After studying the costs of a show, the owner discovered several ways of lowering the cost.
; As a result of these improvements, there is no longer a fixed cost, a variable cost of $1.50 per attendee remains.

(profit 1.0)  ;-360
(profit 2.0)  ;285
(profit 3.0)  ;630
(profit 4.0)  ;675
(profit 5.0)  ;420

;; Exercise 30
; Define constants for the price optimization program at the movie theater so that the price sensitivity of attendance
; (15 people for every 10 cents) becomes a computed constant.
