;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 1-10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; HTDP

;; Exercise 1

; Imagine that x and y are the coordinates of a Cartesian point.
; Write down an expression that computes the distance of this point to the origin,
; that is, a point with the coordinates (0,0)
(define x 3)
(define y 4)

(define distance (sqrt(+ (sqr x) (sqr y))))



;; Exercise 2
; use string primitives to create an expression that concatenates prefix and suffix and adds "_" between them.
; When you run this program, you will see "hello_world" in the interactions area.
(define prefix "hello")
(define suffix "world")

(define appended (string-append prefix "_" suffix))


;; Exercise 3
; create an expression using string primitives that adds "_" at position i.
; In general this means the resulting string is longer than the original one,
; here the expected result is "hello_world".

(define str "helloworld")
(define i 5)

(define result (string-append (substring str 0 i) "_" (substring str i (string-length str))))


;; Exercise 4
; Use the same setup as in exercise 3 to create an expression that deletes the ith position from str.
; Clearly this expression creates a shorter string than the given one. Which values for i are legitimate?

(define str2 "helloworld")
(define i2 5)

(define shorter (string-append (substring str2 0 (- i2 1)) (substring str2 i2 (string-length str2))))

;; Exercise 5
; Use the 2htdp/image library to create the image of a simple boat or tree.
; Make sure you can easily change the scale of the entire image. 

(define tree (above (circle 40 "solid" "green")
                    (rectangle 20 60 "solid" "brown")))
;; Exercise 6
; Create an expression that counts the number of pixels in the image.
; substitute image with cat: https://www.htdp.org/2019-02-24/cat1.png

(define cat (square 10 red))
(define (size img) (* (image-width img) (image-height img)))

;; Exercise 7
; Now create an expression that computes whether sunny is false or friday is true.
; So in this particular case, the answer is false. (Why?) 
(define sunny #true)
(define friday #false)

(define weather-bool (or (not sunny) friday))

;; Exercise 8
; Create a conditional expression that computes whether the image is tall or wide.
; An image should be labeled "tall" if its height is larger than or equal to its width; otherwise it is "wide".

(define (tall? img) (if (> (image-height img)
                           (image-width img))
                        "tall"
                        "wide"))


; Now try the following modification.
; Create an expression that computes whether a picture is "tall", "wide", or "square".

(define (tall-wide img)
  (cond [(> (image-height img)
            (image-width img))
         "tall"]
        [(> (image-width img)
            (image-height img))
         "wide"]
        [else
         "square"]))

;; Exercise 9
; create an expression that converts the value of in to a positive number.
; For a String, it determines how long the String is
; for an Image, it uses the area
; for a Number, it decrements the number by 1, unless it is already 0 or negative
; for #true it uses 10 and for #false 20

(define (in val)
  (cond[(number? val)
        (if (> val 0)
        (- val 1)
        val)]
       [(string? val)
        (string-length val)]
       [(image? val)
        (size val)]
       [(boolean? val)
        (if val
        10
        20)]))

;; Exercise 10
;  Now relax, eat, sleep, and then tackle the next chapter. 