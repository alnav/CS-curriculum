;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |htdp 191-194|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; a plain background image 
(define MT (empty-scene 50 50))
 
; Image Polygon -> Image
; renders the given polygon p into img
#;
(define (render-poly img p)
  img)

(define triangle-p  (list
                     (make-posn 20 10)
                     (make-posn 20 20)
                     (make-posn 30 20)))
	
(define square-p  (list
                   (make-posn 10 10)
                   (make-posn 20 10)
                   (make-posn 20 20)
                   (make-posn 10 20)     
                   ))

(check-expect (render-polygon MT triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))

(check-expect
 (render-polygon MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

; Image Posn Posn -> Image 
; renders a line from p to q into img
(check-expect (render-line MT (make-posn 10 20) (make-posn 20 30)) (scene+line MT 10 20 20 30 "red"))

(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

;; Exercise 191.
;Adapt the second example for the render-poly function to connect-dots.

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

(define (connect-dots img p)
  (cond
    [(empty? (rest p)) MT]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))

; Image Polygon -> Image 
; adds an image of p to MT
(define (render-polygon img p)
  (render-line (connect-dots img p) (first p) (last p)))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

;; Exercise 192.
;Argue why it is acceptable to use last on Polygons.
;Also argue why you may adapt the template for connect-dots to last:
;Finally, develop examples for last, turn them into tests, and ensure that the
;definition of last in figure 73 works on your examples. 
(check-expect (last (list 3 12 2)) 2)
(check-expect (last (list "a" "b" "c")) "c")

;; Exercise 193. Here are two more ideas for defining render-poly:
;render-poly could cons the last item of p onto p and then call connect-dots.
;render-poly could add the first item of p to the end of p via a version of add-at-end that works on Polygons.
;Use both ideas to define render-poly; make sure both definitions pass the test cases.

(define (render-poly-c img p)
  (connect-dots img (cons (last p) p)))

(define (render-poly-a img p)
  (connect-dots img (add-at-end p)))


(check-expect (add-at-end (list 3 2 1)) (list 3 2 1 3))
(define (add-at-end p)
  (append p (list (first p))))

;; Exercise 194.
;Modify connect-dots so that it consumes an additional Posn to which the last Posn is connected.
;Then modify render-poly to use this new version of connect-dots.

(define (connect-dots.v2 img p last-p)
  (cond
    [(empty? (rest p)) MT]
    [else (render-line
           (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))
           last-p
           (first p))]))

(define (render-poly.v2 img p)
  (connect-dots.v2 img p (last p)))


