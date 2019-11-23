;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |htdp 250-|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
; ---------------
;; Exercise 250.
; ---------------
;Design tabulate, which is the abstraction of the two functions in figure 92.
;When tabulate is properly designed, use it to define a tabulation function for sqr and tan.

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
  

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; Number [Number -> Number] -> List-of-Number
(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n) f))]))

(define (tabulate-sin n)
  (tabulate n sin))

(define (tabulate-sqrt n)
  (tabulate n sqrt))

; ---------------
;; Exercise 251.
; ---------------
;Design fold1, which is the abstraction of the two functions in figure 93.

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))
  

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [List-of Number] [Number Number -> Number] -> Number
(define (fold1 l f) 
  (cond
    [(empty? l) 1]
    [else
     (f (first l)
        (fold1 (rest l) f))]))

; ---------------
;; Exercise 252.
; ---------------
;Design fold2, which is the abstraction of the two functions in figure 94.
;Compare this exercise with exercise 251. Even though both involve the product function, this exercise poses an additional challenge
;because the second function, image*, consumes a list of Posns and produces an Image.

; [List-of Number] -> Number;
#;
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))
  

; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

(define (fold2 l f end)
  (cond
    [(empty? l) end]
    [else
     (f (first l)
        (fold2 (rest l) f end))]))

(define L1 (list (make-posn 10 20) (make-posn 45 80) (make-posn 20 50)))

; ---------------
;; Exercise 253.
; ---------------
;Each of these signatures describes a class of functions:
; [Number -> Boolean]
(define (numbool n)
  (> n 5))

; [Boolean String -> Boolean]
(define (boolstr b s)
  (and b (< (string-length s) 3)))

; [Number Number Number -> Number]
(define (sum3 n1 n2 n3)
  (+ n1 n2 n3))

; [Number -> [List-of Number]]
(define (numlist n)
  (cond
    [(<= n 0) '()]
    [else
     (cons n (numlist (- n 1)))]))

; [[List-of Number] -> Boolean]
(define (listnumbool l)
  (cond
    [(empty? l) #true]
    [else
     (and (> (first l) 0) (listnumbool (rest l)))]))

;Describe these collections with at least one example from ISL.

; ---------------
;; Exercise 254.
; ---------------
;Formulate signatures for the following functions:
;sort-n, which consumes a list of numbers
;and a function that consumes two numbers (from the list) and produces a Boolean
;sort-n produces a sorted list of numbers.

; [List-of Numbers] [Number Number -> Boolean] -> [List-of Numbers]

;sort-s, which consumes a list of strings
;and a function that consumes two strings (from the list) and produces a Boolean;
;sort-s produces a sorted list of strings.

; [List-of Strings] [String String -> Boolean] -> [List-of Strings]


;Then abstract over the two signatures, following the above steps.

; [X Y] [List-of X] [X X -> Y] -> [List-of X]

;Also show that the generalized signature can be instantiated to describe the signature of a sort function for lists of IRs.

; [List-of IR] [IR IR -> Boolean] -> [List-of IR]

; ---------------
;; Exercise 255.
; ---------------
;Formulate signatures for the following functions:
;map-n, which consumes a list of numbers and a function from numbers to numbers to produce a list of numbers.

; [List-of Numbers] [Number -> Number] -> [List-of Numbers]

;map-s, which consumes a list of strings and a function from strings to strings and produces a list of strings.

; [List-of Strings] [String -> String] -> [List-of Strings]

;Then abstract over the two signatures, following the above steps.

; [X Y] [List-of X] [X -> Y] -> [List-of Y]

;Also show that the generalized signature can be instantiated to describe the signature of the map1 function above.

; signature for map1:
; [X Y] [List-of X] [X -> Y] -> [List-of Y]

; ---------------
;; Exercise 256.
; ---------------
;Explain the following abstract function:
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
;(define (argmax f lx) ...)
;Use it on concrete examples in ISL. Can you articulate an analogous purpose statement for argmin?
;(define (add3 n) (+ n 3))
;(argmax add3 (list 2 3 8 4 9)) -> 9

; purpose statement for argmin
; finds the (first) item in a list that minimises the function

; ---------------
;; Exercise 257.
; ---------------

;You can design build-list and foldl with the design recipes that you know, but they are not going to be like the ones that ISL provides.
;For example, the design of your own foldl function requires a use of the list reverse function:
; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))
(define (f*oldl f e l)
  (foldr f e (reverse l)))

;Design build-l*st, which works just like build-list.
;Hint Recall the add-at-end function from exercise 193.
;Note on Design Accumulators covers the concepts needed to design these functions from scratch.

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-list n f) == (list (f 0) ... (f (- n 1)))
;(define (build-list n f) ...)

(define (build-l*st n f)
  (reverse (bl n f)))

(define (bl n f)
  (cond
    [(< n 1) '()]
    [else
     (cons (f (sub1 n)) (bl (sub1 n) f))]))

; ---------------
;; Exercise 258.
; ---------------
;Use a local expression to organize the functions for drawing a polygon in figure 73.
;If a globally defined function is widely useful, do not make it local.

(define (render-polygon img p)
  (local ((define MT (empty-scene 400 400))
          ; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) MT]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))
          ; Image Posn Posn -> Image 
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))])))
    (render-line (connect-dots img p) (first p) (last p))))



          