;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 76-87|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; HTDP

;; Exercise 76
; Formulate data definitions for the following structure type definitions:
(define-struct movie [title producer year])
; A Movie is a structure: 
;   (make-movie String String Natural)
; interpretation (make-movie title producer year)
; movie with a title, producer and year

(define-struct person [name hair eyes phone])
; (make-person String String String Natural)

(define-struct pet [name number])
; (make-pet String Natural)

(define-struct CD [artist title price])
; (make-CD String String Number)

(define-struct sweater [material size producer])
; (make-sweater String String[xs,s,m,l,xl], String)

; Make sensible assumptions as to what kind of values go into each field.


;; Exercise 77
; Provide a structure type definition and a data definition
; for representing points in time since midnight.
; A point in time consists of three numbers: hours, minutes, and seconds.

(define-struct times [hours minutes seconds])
; A time is a structure:
; (make-time Natural[1,23] Natural[0,59] Natural[0,59])
; interp. time since midnight, made of hours, minutes and seconds

;; Exercise 78
; Provide a structure type and a data definition for representing three-letter words.
; A word consists of lowercase letters, represented with the 1Strings "a" through "z" plus #false.

;; Letter is one of:
;; 1String[a,z]
;; #false

(define-struct word [l1 l2 l3])
;(make-word letter letter letter)

;; Exercise 79
; Create examples for the following data definitions:
; A Color is one of:
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"
(define-struct colors [col])

(define c1 (make-colors "white"))

;; Sample problem:
; Design a function that computes the distance of objects in a 3-dimensional space to the origin
(define-struct r3 [x y z])
; An R3 is a structure:
;   (make-r3 Number Number Number)
 
(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))

(check-within (r3-distance-to-0 ex1) (sqrt (+ (sqr (r3-x ex1))
                                              (sqr (r3-y ex1))
                                              (sqr (r3-z ex1)))) 0.1)

;; r3 -> Number
;; compute distance of r3 from origin
(define (r3-distance-to-0 p)
  (sqrt (+ (sqr (r3-x p))
           (sqr (r3-y p))
           (sqr (r3-z p)))))

;; Exercise 80
;Create templates for functions that consume instances of the following structure types:
;(define-struct movie [title director year])
;
;(define (fn-for-movie m)
;  (... (movie-title m)
;       (movie-director m)
;       (movie-year m)))
;...
;(define-struct pet [name number])
;
;(define-struct CD [artist title price])
;
;(define-struct sweater [material size color])

;; Exercise 81
; Design the function time->seconds, which consumes instances of time structures
; (see exercise 77) and produces the number of seconds that have passed since midnight.
; For example, if you are representing 12 hours, 30 minutes, and 2 seconds
; with one of these structures and if you then apply time->seconds to this instance,
; the correct result is 45002.

(define-struct time1 [hours minutes seconds])
; A time is a structure:
; (make-time Natural[1,23] Natural[0,59] Natural[0,59])
; interp. time since midnight, made of hours, minutes and seconds

;; Time -> Natural
;; Produces seconds since midnight
(check-expect (time->seconds (make-time1 0 0 10)) 10)
(check-expect (time->seconds (make-time1 0 2 15)) 135)
(check-expect (time->seconds (make-time1 1 0 30)) 3630)

;(define (time->seconds t) 0) ;stub

(define (time->seconds t)
  (+ (* (time1-hours t) 3600)
     (* (time1-minutes t) 60)
     (time1-seconds t)))

;; Exercise 81
;Design the function compare-word.
;The function consumes two three-letter words (see exercise 78).
;It produces a word that indicates where the given ones agree and disagree.
;The function retains the content of the structure fields if the two agree;
;otherwise it places #false in the field of the resulting word.
;Hint The exercises mentions two tasks:
;the comparison of words and the comparison of “letters.”
;(define-struct word [l1 l2 l3])

;; Word Word -> Word
;; consumes 2 words:
;; make new Word with either letter if both words have same letter in same place, or false in that space
(check-expect (compare-words (make-word "a" "b" "c") (make-word "a" "b" "c")) (make-word "a" "b" "c"))
(check-expect (compare-words (make-word "a" "x" "c") (make-word "a" "b" "c")) (make-word "a" #false "c"))
(check-expect (compare-words (make-word "z" "x" "s") (make-word "a" "b" "c")) (make-word #false #false #false))

(define (compare-words w1 w2)
  (make-word (if (equal-1String? (word-l1 w1) (word-l1 w2))
                 (word-l1 w1)
                 #false)
             (if (equal-1String? (word-l2 w1) (word-l2 w2))
                 (word-l2 w1)
                 #false)
             (if (equal-1String? (word-l3 w1) (word-l3 w2))
                 (word-l3 w1)
                 #false)))

;; 1String 1String -> Boolean
;; Compare 2 1String, return true if equal, false if not
(check-expect (equal-1String? "a" "a") #true)
(check-expect (equal-1String? "x" "a") #false)

(define (equal-1String? s1 s2)
  (string=? s1 s2))


;; Exercise 83
; Design the function render, which consumes an Editor and produces an image.

;; Exercise 84
; Design edit. The function consumes two inputs, an editor ed and a KeyEvent ke, and it produces another editor.
;Its task is to add a single-character KeyEvent ke to the end of the pre field of ed,
;unless ke denotes the backspace ("\b") key.
;In that case, it deletes the character immediately to the left of the cursor (if there are any).
;The function ignores the tab key ("\t") and the return key ("\r").

;; Exercise 85
;Define the function run. Given the pre field of an editor, it launches an interactive editor,
;using render and edit from the preceding two exercises for the to-draw and on-key clauses, respectively.

;; Exercise 86
;Notice that if you type a lot, your editor program does not display all of the text.
;Instead the text is cut off at the right margin.
;Modify your function edit from exercise 84 so that it ignores a keystroke if adding
;it to the end of the pre field would mean the rendered text is too wide for your canvas.

;; Exercise 87
;Develop a data representation for an editor based on our first idea, using a string and an index.
;Then solve the preceding exercises again. Retrace the design recipe.

;; A simple editor

;; Constants
;; =========

(define WIDTH 1000)
(define HEIGHT 32)
(define MTS (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 2 24 "solid" "red"))

(define TEXT-SIZE 24)
(define TEXT-COLOUR "black")

;; Data Definitions
;; ================

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))
(define E1 (make-editor "a" ""))
(define E2 (make-editor "" "b"))

#;
(define (fn-for-editor e)
  (... (editor-pre e)
       (editor-post e)))

;; Functions
;; ================

;; Simple text editor program

;; Editor -> Editor
;; start the world with (make-editor "" "")
;;

(define (main e)
  (big-bang e                   
    (on-key    update)     ; Editor -> Editor
    (to-draw   render)))   ; Editor -> Image
       


;; editor-pre editor-post -> editor-pre editor-post
;; Moves cursor to the left
;; !!! Add conditional if pre is empty
(define (move-cursor-left pre post)
  (make-editor
   (substring pre 0 (- (string-length pre) 1))
   (string-append (substring pre (- (string-length pre) 1) (string-length pre)) post)))


;; editor-pre editor-post -> editor-pre editor-post
;; Moves cursor to the right
;; !!! Add conditional if post is empty
(define (move-cursor-right pre post)
  (make-editor
   (string-append pre (substring post 0 1))
   (substring post 1 (string-length post))))


;; editor-pre editor-post -> editor-pre editor-post
;; Delete character on the left of the cursor
;; !!! Add conditional if pre is empty
(define (delete-char pre post)
  (make-editor
   (substring pre 0 (- (string-length pre) 1))
   post))

;; Editor -> Boolean
;; Return true if image-width of text is longer than screen

(define (outside-range? e)
  (> (+ (image-width (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)) (image-width (text (editor-post e) TEXT-SIZE TEXT-COLOUR))) (- WIDTH 20)))
 
;; Editor -> Editor
;; Update editor state if any text is entered
(check-expect (update (make-editor "" "") "a") (make-editor "a" ""))
(check-expect (update (make-editor "kal" "") "e") (make-editor "kale" ""))
(check-expect (update (make-editor "12 " "") "3") (make-editor "12 3" ""))

(define (update e ke)
  (cond [(outside-range? e) e]
        [(key=? ke "left") (move-cursor-left (editor-pre e) (editor-post e))]
        [(key=? ke "right") (move-cursor-right (editor-pre e) (editor-post e))]
        [(key=? ke "\b") (delete-char (editor-pre e) (editor-post e))]
        [(key=? ke "shift") e]
        [(key=? ke "rshift") e]
        [(key-event? ke)
         (make-editor (string-append (editor-pre e) ke) (editor-post e))]))


;; Editor -> Image
;; Renders editor after update
#;
(check-expect (render (make-editor "" "")) (place-image (beside
                                                         (text "" TEXT-SIZE TEXT-COLOUR)
                                                         CURSOR (text "" TEXT-SIZE TEXT-COLOUR))
                                                        50 50 MTS))
#;
(check-expect (render (make-editor "ale" " post")) (place-image (beside
                                                                 (text "ale" TEXT-SIZE TEXT-COLOUR)
                                                                 CURSOR
                                                                 (text " post" TEXT-SIZE TEXT-COLOUR))
                                                                50 50 MTS))

#;
(define (render e) 
  (place-image (beside
                (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                CURSOR
                (text (editor-post e) TEXT-SIZE TEXT-COLOUR))
               (+ 20 (* 5.5 (+ (string-length (editor-pre e)) (string-length (editor-post e))))) 50 MTS))

(define (render e) 
  (overlay/align "left" "center"
                 (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                         CURSOR
                         (text (editor-post e) TEXT-SIZE TEXT-COLOUR)) MTS))
                

(main (make-editor "" ""))