;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 166-176|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Exercise 166.
;The wage*.v2 function consumes a list of work records and produces a list of numbers.
;Of course, functions may also produce lists of structures.

;Develop a data representation for paychecks. Assume that a paycheck contains two distinctive pieces of information:
;the employee’s name and an amount. Then design the function wage*.v3.
;It consumes a list of work records and computes a list of paychecks from it, one per record.

;(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

;(define-struct paycheck [name amount])
; Paycheck is a structure:
; - (make-paycheck String Number)
; interp. name of the employee, amount paid

; list-of-paychecks is one of:
; - empty
; - (cons paycheck lop)

; low -> lop
#;
(check-expect (wage*3.v3 (cons (make-work "Robby" 11.95 39) '())) (cons (make-paycheck "Robby" (* 11.95 39)) '()))
#;
(define (wage*3.v3 low)
  (cond
    [(empty? low) '()]
    [else
     (cons (p-check (first low)) (wage*3.v3 (rest low)))]))

#;
(define (p-check w)
  (make-paycheck (work-employee w) (* (work-rate w) (work-hours w))))



;In reality, a paycheck also contains an employee number.
;Develop a data representation for employee information and change the data definition for work records
;so that it uses employee information and not just a string for the employee’s name.
;Also change your data representation of paychecks so that it contains an employee’s name and number, too.
;Finally, design wage*.v4, a function that maps lists of revised work records to lists of revised paychecks.

(define-struct em-info [name number])
; an employee-information is a structure:
; (make-em-info String Number)

(define-struct work [em-info rate hours])

(define-struct paycheck [name number amount])

(check-expect (wage*.v4 (cons (make-work (make-em-info "ale" 230293) 23 452) '())) (list (make-paycheck "ale" 230293 10396))) 
(define (wage*.v4 low)
    (cond
    [(empty? low) '()]
    [else
     (cons (p-check2 (first low)) (wage*.v4 (rest low)))]))

(define (p-check2 w)
  (make-paycheck (em-info-name (work-em-info w)) (em-info-number (work-em-info w)) (* (work-rate w) (work-hours w))))


;; Exercise 167.
;Design the function sum, which consumes a list of Posns and produces the sum of all of its x-coordinates.
(check-expect (sum (cons (make-posn 3 4) (cons (make-posn 9 4) '()))) 12)

; list-of-posns -> number
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else
     (+ (posn-x (first lop)) (sum (rest lop)))]))


;; Exercise 168.
;Design the function translate. It consumes and produces lists of Posns.
;For each (make-posn x y) in the former, the latter contains (make-posn x (+ y 1)).
;We borrow the word “translate” from geometry, where the movement of a point by a constant distance
;along a straight line is called a translation.

(check-expect (translate (cons (make-posn 3 4) (cons (make-posn 9 4) '()))) (cons (make-posn 3 5) (cons (make-posn 9 5) '())))

; list-of-posns -> list-of-posns
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else
     (cons (make-posn (posn-x (first lop))
                      (+ (posn-y (first lop)) 1))
           (translate (rest lop)))]))


;; Exercise 169.
;Design the function legal.
;Like translate from exercise 168, the function consumes and produces a list of Posns.
;The result contains all those Posns whose x-coordinates are between 0 and 100
;and whose y-coordinates are between 0 and 200.
(check-expect (legal (cons (make-posn 3 4) (cons (make-posn 900 4) '()))) (cons (make-posn 3 4) '()))

; list-of-posns -> list-of-posns
(define (legal lop)
  (cond
  [(empty? lop) '()]
  [(valid? (first lop)) (cons (first lop) (legal (rest lop)))]
  [else (legal (rest lop))]))

(check-expect (valid? (make-posn 4 9)) #true)
(check-expect (valid? (make-posn 230 89)) #false)

; posn -> Boolean
(define (valid? p)
  (cond
    [(or (< (posn-x p) 0) (> (posn-x p) 100)) #false]
    [(or (< (posn-y p) 0) (> (posn-y p) 200)) #false]
    [else #true]))


;; Exercise 170. Here is one way to represent a phone number:
(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999. 

;Design the function replace. It consumes and produces a list of Phones.
;It replaces all occurrence of area code 713 with 281.
(check-expect (replace (cons (make-phone 713 492 2838) '())) (cons (make-phone 281 492 2838) '()))

(define (replace lopn)
    (cond
      [(empty? lopn) '()]
      [(= (phone-area (first lopn)) 713)
       (cons (make-phone 281
                         (phone-switch (first lopn))
                         (phone-four (first lopn)))
             (replace (rest lopn)))]
      [else (replace (rest lopn))]))
  

;; Exercise 171.
;You know what the data definition for List-of-strings looks like. Spell it out.
;Make sure that you can represent Piet Hein’s poem as an instance of the definition where each line
;is represented as a string and another instance where each word is a string.
;Use read-lines and read-words to confirm your representation choices.

; A list-of-strings is one of:
; - empty
; - (cons String list-of-string)

(define ttt (read-file "ttt.txt"))

;Next develop the data definition for List-of-list-of-strings.
;Again, represent Piet Hein’s poem as an instance of the definition where each line is
;represented as a list of strings, one per word, and the entire poem is a list of such line representations.
;You may use read-words/line to confirm your choice. 

; A list-of-list-of-strings is one of:
; empty
; - (cons list-of-strings list-of-list-of-strings)


; An LLS is one of: 
; – '()
; – (cons Los LLS)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
(define line2 (cons "this" (cons "is" (cons "a" (cons "line" '())))))
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(define lls2 (cons line0 (cons line1 (cons line2 '()))))
 
; LLS -> List-of-numbers
; determines the number of words on each line 
 
(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1) (cons 2 (cons 0 '())))
 
(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))


;; Exercise 172.
;Design the function collapse, which converts a list of lines into a string.
;The strings should be separated by blank spaces (" "). The lines should be separated with a newline ("\n").

;Challenge When you are finished, use the program like this:
;(write-file "ttt.dat"
;            (collapse (read-words/line "ttt.txt")))
;To make sure the two files "ttt.dat" and "ttt.txt" are identical,
;remove all extraneous white spaces in your version of the T.T.T. poem.

; list-of-lines -> string
; converts a list of lines into a string
;(define (collapse) ...)


(check-expect (collapse lls1) "hello world \n\n")
(check-expect (collapse lls2) "hello world \n\nthis is a line \n")

#;
(define (collapse lls)
  (cond
    [(empty? lls) ...]
    [else (... (first lls) ... (rest lls) ...)]))
#;
(define (collapse lls)
  (cond
    [(empty? lls) ...]
    [else (string-append (join-line (first lls) ... (rest lls) ...))]))

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (join-line (first lls))
                         "\n"
                         (collapse (rest lls)))]))

; !!! TO FIX: space after last word
; list-of-strings -> string
; append strings in a list-of-strings
(check-expect (join-line line0) "hello world ")
(check-expect (join-line line2) "this is a line ")

(define (join-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los) " " (join-line (rest los)))]))

(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))


;; Exercise 173.
;Design a program that removes all articles from a text file.
;The program consumes the name n of a file, reads the file, removes the articles,
;and writes the result out to a file whose name is the result of concatenating "no-articles-" with n.
;For this exercise, an article is one of the following three words: "a", "an", and "the".

;Use read-words/line so that the transformation retains the organization of the original text
;into lines and words. When the program is designed, run it on the Piet Hein poem. 


; String -> Boolean
; check if a string is an article
(define (isArticle? str)
  (or (string=? str "a") (string=? str "an") (string=? str "the")))

(define (no-articles lls)
  (cond
    [(empty? lls) '()]
    [else (cons (remove-articles (first lls))
                (no-articles (rest lls)))]))

(define (remove-articles los)
  (cond
    [(empty? los) '()]
    [(isArticle? (first los)) (cons "" (remove-articles (rest los)))]
    [else (cons (first los) (remove-articles (rest los)))]))

(define (no-art n)
  (write-file (string-append "no-articles-" n ".txt") (collapse (no-articles (read-words/line n)))))

;; Exercise 174.
;Design a program that encodes text files numerically.
;Each letter in a word should be encoded as a numeric three-letter string with a value between 0 and 256.
;Figure 69 shows our encoding function for single letters. Before you start, explain these functions.

;Hints (1) Use read-words/line to preserve the organization of the file into lines and words.
;(2) Read up on explode again.

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

; String -> list-of-codes
; converts a string into a list of encoded 1Strings
(check-expect (encode-str "ale") (cons (encode-letter "a")
                                       (cons (encode-letter "l")
                                             (cons (encode-letter "e") '()))))

(define (encode-str str)
  (cond
    [(empty? (explode str)) '()]
    [else
     (cons (encode-letter (first (explode str)))
           (encode-str (substring str 1)))]))


; list-of-strings -> list-of-list-of-codes
(check-expect (encode-los (cons "ab" (cons "cd" '())))
              (cons (encode-str "ab") (cons (encode-str "cd") '())))

(define (encode-los los)
  (cond
    [(empty? los) '()]
    [else
     (cons (encode-str (first los))
           (encode-los (rest los)))]))


; list-of-lines -> list-of-lines
; converts list-of-lines from words to numeric code

(define (encode-lol ln)
  (cond
    [(empty? ln) '()]
    [else
     (append (encode-los (first ln))
             (encode-lol (rest ln)))]))

(define encoded (encode-lol (read-words/line "ttt.txt")))

; FINAL OUTPUT
(write-file "encoded.txt" (collapse encoded))


;; Exercise 175.
;Design a BSL program that simulates the Unix command wc.
;The purpose of the command is to count the number of 1Strings, words, and lines in a given file.
;That is, the command consumes the name of a file and produces a value that consists of three numbers.

(define-struct wc-num [1s w l])
; wc-num is a Number Number Number:
; 1s is 1String
; w is words
; l is lines
; interp. 1strings, words and lines in a file

; string -> number
; number of 1Str in a string
(define (1s str)
  (string-length str))

; list-of-strings -> number
; number of 1Str in a list of strings
(define (1s-los los)
  (cond
    [(empty? los) 1]
    [else
     (+ (1s (first los)) (1s-los (rest los)))]))

; list-of-list-of-strings -> number
; number of 1Str in a list of list of strings
(define (1s-lol lol)
  (cond
   [(empty? lol) 1]
   [else
    (+ (1s-los (first lol)) (1s-lol (rest lol)))]))

; list-of-lines -> number
; number of lines in a list of lines
(define (wc-l lol)
  (cond
    [(empty? lol) 0]
    [else
     (+ 1 (wc-l (rest lol)))]))

(define (wc-w lol)
  (cond
    [(empty? lol) 0]
    [else
     (+ (wc-l (first lol)) (wc-w (rest lol)))]))

(define text (read-words/line "ttt2.txt"))

; File -> wc-num
; wc command: returns a wc object with number of 1str, words and lines in a number

(define (wc file) (make-wc-num (1s-lol (read-words/line file))
                               (wc-w (read-words/line file))
                               (wc-l (read-words/line file))))

;; Exercise 176.
;Mathematics teachers may have introduced you to matrix calculations by now.
;In principle, matrix just means rectangle of numbers. Here is one possible data representation for matrices:

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 
(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; Matrix -> Row
; returns a row with first column of the matrix
(check-expect (first* (list (list 11 12) (list 21 22)))
              (cons 11 (cons 21 '())))

(define (first* m)
  (cond
    [(empty? m) '()]
    [else (cons (first (first m))
                (first* (rest m)))]))

; Matrix -> Row
; removes the first colunmn of a matrix
(define (rest* m)
  (cond
    [(empty? m) '()]
    [else (cons (rest (first m))
                (rest* (rest m)))]))
