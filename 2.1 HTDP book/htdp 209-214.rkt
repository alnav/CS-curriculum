;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 209-2014|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; ----------------
;; Exercise 209.
; ----------------
;The above leaves us with two additional wishes: a function that consumes a
;String and produces its corresponding Word, and a function for the opposite direction.
;Here are the wish-list entries:

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "rat") (list "r" "a" "t"))
(define (string->word s)
  (explode s))
 
; Word -> String
; converts w to a string
(check-expect (word->string (list "c" "a" "t")) "cat")

(define (word->string w) (implode w))

;Look up the data definition for Word in the next section and complete the definitions
;of string->word and word->string. Hint You may wish to look in the list of functions that BSL provides.

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))

; String -> List-of-strings
; finds all words that the letters of some given word spell
#; 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
#;
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
#;
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))

; ----------------
;; Exercise 210.
; ----------------
;Complete the design of the words->strings function specified in figure 78.
;Hint Use your solution to exercise 209.

; List-of-words -> List-of-strings
(check-expect (words->strings (list (list "c" "a" "t") (list "d" "o" "g"))) (list "cat" "dog"))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else
     (cons (word->string (first low)) (words->strings (rest low)))]))

; ----------------
;; Exercise 211.
; ----------------
;Complete the design of in-dictionary, specified in figure 78.
;Hint See Real-World Data: Dictionaries for how to read a dictionary.
(define AS-LIST (read-lines "words-short.txt"))


; List-of-strings -> List-of-strings
(define (in-dictionary los d)
  (cond
    [(empty? los) '()]
    [(member? (first los) d)
     (cons (first los) (in-dictionary (rest los) d))]
    [else
     (in-dictionary (rest los) d)]))


; ----------------
;; Exercise 212.
; ----------------
;Write down the data definition for List-of-words.
;Make up examples of Words and List-of-words.
;Finally, formulate the functional example from above with check-expect.
;Instead of the full example, consider working with a word of just two letters, say "d" and "e".

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '() or
; - (cons Word List-of-words)
; a list of words

(define w1 (list "c" "a" "t"))
(define w2 (list "d" "o" "g"))
(define w3 (list "r" "a" "t"))

(define low1 (list w1 w2))
(define low2 (list w2 w3))
(define low3 (list w1 w2 w3))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

; ----------------
;; Exercise 213.
; ----------------
;Design insert-everywhere/in-all-words.
;It consumes a 1String and a list of words.
;The result is a list of words like its second argument, but with the first argument inserted at the beginning,
;between all letters, and at the end of all words of the given list.

; 1String List-of-words -> List-of-words
; produces a list-of-words with the 1String inserted in between all the letters
(check-expect (insert-everywhere/in-all-words "a" '()) '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "b"))) (list (list "b" "a") (list "a" "b")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "b" "c"))) (list (list "b" "c" "a") (list "b" "a" "c") (list "a" "b" "c")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "b") (list "c"))) (list (list "b" "a") (list "a" "b") (list "c" "a") (list "a" "c")))

#;
(define (insert-everywhere/in-all-words l low)
  (cond
    [(empty? low) '()]
    [else
     (... (first low)
          ... (insert-everywhere/in-all-words l (rest low)))]))

(define (insert-everywhere/in-all-words l low)
  (cond
    [(empty? low) '()]
    [else
     (append (insert l (length (first low)) (first low))
          (insert-everywhere/in-all-words l (rest low)))]))

;1String Word -> List-of-words
; produces a list-of-words, with 1String inserted in between every 1String
(check-expect (insert "a" 0 '()) (list (list "a")))
(check-expect (insert "a" (length (list "b")) (list "b")) (list (list "b" "a") (list "a" "b")))
(check-expect (insert "a" (length (list "b" "c")) (list "b" "c")) (list (list "b" "c" "a") (list "b" "a" "c") (list "a" "b" "c")))

(define (insert l times w)
  (cond
    [(= times -1) '()]
    [else
     (cons (insert-at l times w) (insert l (- times 1) w))]))

; Number List -> List
; slices a list [:pos]
(check-expect (after 2 (list 1 2 3 4)) (list 3 4))

(define (after pos l)
  (cond
  [(= 0 pos) l]
  [else
   (after (- pos 1) (rest l))]))

; Number List -> List
; slices a list [pos:]
(check-expect (before 2 (list 1 2 3 4)) (list 1 2))

(define (before pos l)
  (cond
    [(= 0 pos) '()]
    [else
     (cons (first l) (before (- pos 1) (rest l)))]))


; Any Number List -> List
; Insert a value into a list, at a defined position
(check-expect (insert-at 1 2 (list 2 3 4)) (list 2 3 1 4))

(define (insert-at l pos list)
  (append (append (before pos list) (cons l '())) (after pos list)))


; ----------------
;; Exercise 214.
; ----------------
;Integrate arrangements with the partial program from Word Games, Composition Illustrated.
;After making sure that the entire suite of tests passes, run it on some of your favorite examples.

; (arrangements (list "c" "a" "t"))