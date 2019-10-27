;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |htdp 195-198|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
;(define WORDS (read-lines "words.txt"))
(define AS-LIST (read-lines "words-short.txt"))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

;; Exercise 195.
;Design the function starts-with#, which consumes a Letter and Dictionary and then
;counts how many words in the given Dictionary start with the given Letter.
;Once you know that your function works, determine how many words start with "e" in your computer’s
;dictionary and how many with "z".

; Letter Dictionary -> Number
; interp. counts how many words in the dictionary start with a given letter

;(define (starts-with# "a" dict) ...)

(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [(first-letter? letter (first dict))
     (+ 1 (starts-with# letter (rest dict)))]
    [else
     (starts-with# letter (rest dict))]))

; letter word -> boolean
(define (first-letter? l w)
  (string=? l (first (explode w))))


;; Exercise 196.
;Design count-by-letter. The function consumes a Dictionary and counts how often each letter is used
;as the first one of a word in the given dictionary. Its result is a list of Letter-Counts,
;a piece of data that combines letters and counts.

(define-struct letter-count [letter count])
; A letter count is a (make-letter-count [1String Number])
; interp. a letter with how many times it appears in the dictionary

; Dictionary -> Letter-Counts
; interp. returns a list of how many words start with each letter, in a dictionary

(define (count-by-letter letters dict)
  (cond
  [(empty? letters) '()]
  [(cons? letters)
   (cons (make-letter-count (first letters)
                            (starts-with# (first letters) dict))
         (count-by-letter (rest letters) dict))]))

;Once your function is designed, determine how many words appear for all letters in your computer’s dictionary.

;; Exercise 197.
;Design most-frequent. The function consumes a Dictionary.
;It produces the Letter-Count for the letter that occurs most often as the first one in the given Dictionary.

(define (higher-count? lc1 lc2)
  (>= (letter-count-count lc1) (letter-count-count lc2)))

(define (sort-lc lc-list)
  (cond
    [(empty? lc-list) '()]
    [else
     (insert (first lc-list) (sort-lc (rest lc-list)))]))

(define (insert lc lc-list)
  (cond
  [(empty? lc-list) (cons lc '())]
  [else (if (higher-count? lc (first lc-list))
            (cons lc lc-list)
            (cons (first lc-list) (insert lc (rest lc-list))))]))

; solution (slow to compute):
;(define MOST-COMMON (first (sort-lc (count-by-letter LETTERS AS-LIST))))
(define (most-frequent dict) (first (sort-lc (count-by-letter LETTERS dict))))


;; Exercise 198.
;Design words-by-first-letter.
;The function consumes a Dictionary and produces a list of Dictionaries, one per Letter.

; Dictionary -> list-of-dictionaries
(define (words-by-first-letter letter dict)
  (cond
  [(empty? letter) '()]
  [else
   (cons (words-with# (first letter) dict) (words-by-first-letter (rest letter) dict))]))


(define (words-with# letter dict)
  (cond
    [(empty? dict) '()]
    [else
     (if (first-letter? letter (first dict))
         (cons (first dict) (words-with# letter (rest dict)))
         (words-with# letter (rest dict)))]))
   
;Redesign most-frequent from exercise 197 using this new function.
;Call the new function most-frequent.v2. Once you have completed the design,
;ensure that the two functions compute the same result on your computer’s dictionary:
(check-expect
  (most-frequent AS-LIST)
  (most-frequent.v2 AS-LIST))

; Dictionary -> letter-count
(define (most-frequent.v2 dict)
  (first (sort-lc (letter-count-list LETTERS (words-by-first-letter LETTERS AS-LIST)))))

; letter-list Dictionary -> letter-count-list
(define (letter-count-list l dict)
  (cond
    [(empty? dict) '()]
    [else
     (cons (make-letter-count (first l) (how-many (first dict)))
           (letter-count-list (rest l) (rest dict)))]))

; list -> Natural
(define (how-many l)
  (cond
    [(empty? l) 0]
    (else
     (+ 1 (how-many (rest l))))))










     