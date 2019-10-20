;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |htdp 177-180|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
(define r1 (cons "a" (cons "b" (cons "c" '())))) 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 


(define (rev l)
  (cond
    [(empty? l) '()]
    [else
     (add-at-end (rev (rest l))
                 (first l))]))


; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l)
                (add-at-end (rest l) s))]))

;; Exercise 177.
;Design the function create-editor.
;The function consumes two strings and produces an Editor.
;The first string is the text to the left of the cursor and the second string
;is the text to the right of the cursor. The rest of the section relies on this function.

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; String String -> Editor
(check-expect (create-editor "ab" "cd")
              (make-editor (cons "b" (cons "a" '()))
                           (cons "c" (cons "d" '()))))

(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))



(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
;(define (editor-render e) MT)
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

;; Exercise 178.
;Explain why the template for editor-kh deals with "\t" and "\r" before it checks for strings of length 1.
; because return and tab should work when the editor contains an empty string

; Editor 1String -> Editor
; insert the 1String k between pre and post
(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))


(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Exercise 179. Design the functions:

; Editor -> Editor
; moves cursor on the left
(check-expect (editor-lft (make-editor (cons "a" '()) (cons "b" (cons "c" '()))))
              (make-editor '() (cons "a" (cons "b" (cons "c" '())))))
(check-expect (editor-lft (make-editor (cons "b" (cons "a" '())) (cons "c" (cons "d" '()))))
              (make-editor (cons "a" '()) (cons "b" (cons "c" (cons "d" '())))))


(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor (rest (editor-pre ed))
                  (cons (first (editor-pre ed)) (editor-post ed)))]))


; Editor -> Editor
; moves cursor on the right
(check-expect (editor-rgt (make-editor (cons "a" '()) (cons "b" (cons "c" '()))))
              (make-editor (cons "b" (cons "a" '())) (cons "c" '())))

(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else
     (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                  (rest (editor-post ed)))]))

; Editor -> Editor
; deletes character of the left of the cursor
(check-expect (editor-del (make-editor (cons "a" '()) (cons "b" (cons "c" '()))))
              (make-editor '() (cons "b" (cons "c" '()))))
(check-expect (editor-del (make-editor (cons "b" (cons "a" '())) (cons "c" (cons "d" '()))))
              (make-editor (cons "a" '()) (cons "c" (cons "d" '()))))

(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor (rest (editor-pre ed)) (editor-post ed))]))




; Editor -> Image
(define (editor-render e)
  (place-image/align
    (beside (editor-text (rev(editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top" 
    MT))

;; Exercise 180.
;Design editor-text without using implode.

; Lo1s -> Image
; renders a list of 1Strings as a text image
(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))

#;
(define (editor-text s)
  (text "" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (cond
    [(empty? s) (square 0 "solid" "white")]
    [else
     (text (make-text s) FONT-SIZE FONT-COLOR)]))

; Lo1s -> String
(define (make-text s)
  (cond
    [(empty? s) ""]
    [else
     (string-append (first s) (make-text (rest s)))]))
