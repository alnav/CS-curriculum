;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 58-62s|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; HTDP

;; Exercise 58
; Introduce constant definitions that separate the intervals for low prices and luxury prices
; from the others so that the legislators in Tax Land can easily raise the taxes even more.

; Sample Problem The state of Tax Land has created a three-stage sales tax to cope with its budget deficit.
; Inexpensive items, those costing less than $1,000, are not taxed.
; Luxury items, with a price of more than $10,000, are taxed at the rate of eight percent (8.00%).
; Everything in between comes with a five percent (5.00%) markup.

; A Price falls into one of three intervals: 
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item

(define INEXPENSIVE-TAX 0.05)
(define LUXURY-TAX 0.08)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 12017) (* 0.08 12017))


;(define (sales-tax p) 0)

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p 1000)) 0]
    [(and (<= 1000 p) (< p 10000)) (* INEXPENSIVE-TAX p)]
    [(>= p 10000) (* LUXURY-TAX p)]))


;; Exercise 59
; Finish the design of a world program that simulates the traffic light FSA. Here is the main function:

; The function’s argument is the initial state for the big-bang expression,
; which tells DrRacket to redraw the state of the world with tl-render and to handle clock ticks with tl-next.
; Also note that it informs the computer that the clock should tick once per second.
; Complete the design of tl-render and tl-next.
; Start with copying TrafficLight, tl-next, and tl-render into DrRacket’s definitions area.
(define HEIGHT 400)
(define WIDTH 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define RADIUS 8)
(define SQ (square RADIUS "solid" "white"))


(define (c out color) (circle RADIUS out color))

(define RED (overlay (rectangle (* 10 RADIUS) (* 3 RADIUS) "outline" "black")
                     (beside (c "solid" "red")
                             SQ
                             (c "outline" "yellow")
                             SQ
                             (c "outline" "green"))))

(define YELLOW (overlay (rectangle (* 10 RADIUS) (* 3 RADIUS) "outline" "black")
                        (beside (c "outline" "red")
                                SQ
                                (c "solid" "yellow")
                                SQ
                                (c "outline" "green"))))

(define GREEN (overlay (rectangle (* 10 RADIUS) (* 3 RADIUS) "outline" "black")
                       (beside (c "outline" "red")
                               SQ
                               (c "outline" "yellow")
                               SQ
                               (c "solid" "green"))))


; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))



; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next "red") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")

(define (tl-next cs)
  (cond [(string=? cs "red")
         "green"]
        [(string=? cs "green")
         "yellow"]
        [(string=? cs "yellow")
         "red"]))
 
; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render "red") RED)
(check-expect (tl-render "yellow") YELLOW)

(define (tl-render current-state)
  (cond [(string=? current-state "red")
         RED]
        [(string=? current-state "yellow")
         YELLOW]
        [(string=? current-state "green")
         GREEN]))
        
;; Exercise 60
;  An alternative data representation for a traffic light program may use numbers instead of strings:
; An N-TrafficLight is one of:
; – 0 interpretation the traffic light shows red
; – 1 interpretation the traffic light shows green
; – 2 interpretation the traffic light shows yellow
; It greatly simplifies the definition of tl-next:
; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs
 (define (tl-next-numeric cs) (modulo (+ cs 1) 3))
; Reformulate tl-next’s tests for tl-next-numeric.

; Does the tl-next function convey its intention more clearly than the tl-next-numeric function?
; If so, why? If not, why not?

; ANSWER: the function is shorter, but comprehension is more difficult, as this is not as explicit

;; N-TrafficLight -> N-TrafficLight
;; yields the next state, given current state cs
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)

(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

;; Exercise 61
;  As From Functions to Programs says, programs must define constants and use names instead of actual constants.
; In this spirit, a data definition for traffic lights must use constants, too:
; This form of data definition is what a seasoned designer would use.
(define RED 0)
(define GREEN 1)
(define YELLOW 2)
 
; An S-TrafficLight is one of:
; – RED
; – GREEN
; – YELLOW
; If the names are chosen properly, the data definition does not need an interpretation statement.
; S-TrafficLight -> S-TrafficLight
; yields the next state, given current state cs
     

; (check-expect (tl-next- ... RED) YELLOW)
; (check-expect (tl-next- ... YELLOW) GREEN)
     
#;
(define (tl-next-numeric cs)
  (modulo (+ cs 1) 3))
     

(define (tl-next-symbolic cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))
; Figure 27: A symbolic traffic light

; Figure 27 displays two different functions that switch the state of a traffic light in a simulation program.
; Which of the two is properly designed using the recipe for itemization?
; Which of the two continues to work if you change the constants to the following
; (define RED "red")
; (define GREEN "green")
; (define YELLOW "yellow")
; Does this help you answer the questions?

; ANSWER: only tl-next-symbolic would work, because it uses constants names, which can refer to string or
;         numbers

;; Exercise 62
; Design a world program that simulates the working of a door with an automatic door closer.
; If this kind of door is locked, you can unlock it with a key.
; An unlocked door is closed, but someone pushing at the door opens it.
; Once the person has passed through the door and lets go, the automatic door takes over and closes the door again.
; When a door is closed, it can be locked again.



; During a door simulation the “open” state is barely visible.
; Modify door-simulation so that the clock ticks once every three seconds. Rerun the simulation.

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")
   
; A DoorState is one of:
; – LOCKED
; – CLOSED
; – OPEN


;; DoorState -> DoorState
;; World program simulating a door with an automatic door closer

(define (main state-of-door)
  (big-bang state-of-door
    [on-tick door-closer 3]
    [on-key door-action]
    [to-draw door-render]))

; DoorState -> DoorState
; closes an open door over the period of one tick
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)

(define (door-closer state-of-door)
  (cond
    [(string=? LOCKED state-of-door) LOCKED]
    [(string=? CLOSED state-of-door) CLOSED]
    [(string=? OPEN state-of-door) CLOSED]))


;; DoorState KeyEvent -> DoorState
;; turns key event k into an action on door state s
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)
 
(define (door-action s k)
  (cond
    [(and (string=? LOCKED s) (string=? "u" k))
     CLOSED]
    [(and (string=? CLOSED s) (string=? "l" k))
     LOCKED]
    [(and (string=? CLOSED s) (string=? " " k))
     OPEN]
    [else s]))

; DoorState -> Image
; translates the state s into a large text image
(check-expect (door-render CLOSED)
              (text CLOSED 40 "red"))
(define (door-render s)
  (text s 40 "red"))