;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |htdp 181-190|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 181.
;Use list to construct the equivalent of these lists:
(cons "a" (cons "b" (cons "c" (cons "d" '()))))
(list "a" "b" "c" "d")

(cons (cons 1 (cons 2 '())) '())
(list (list 1 2))

(cons "a" (cons (cons 1 '()) (cons #false '())))
(list "a" (list 1) #false)
      
(cons (cons "a" (cons 2 '())) (cons "hello" '()))
(list (list "a" 2) "hello")

;Also try your hand at this one:
(cons (cons 1 (cons 2 '()))
      (cons (cons 2 '())
            '()))
(list (list 1 2) (list 2))

;; Exercise 182.
;Use cons and '() to form the equivalent of these lists:
(list 0 1 2 3 4 5)
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))))

(list (list "he" 0) (list "it" 1) (list "lui" 14))
(cons (cons "he" (cons 0 '()))
      (cons (cons "it" (cons 1 '()))
            (cons (cons "lui" (cons 14 '())) '())))
      
(list 1 (list 1 2) (list 1 2 3))
(cons 1 (cons (cons 1 (cons 2 '()))
              (cons (cons 1 (cons 2 (cons 3 '()))) '())))

;; Exercise 183.
;On some occasions lists are formed with cons and list.
(cons "a" (list 0 #false))
(list "a" 0 #false)

(list (cons 1 (cons 13 '())))
(list (list 1 13))

(cons (list 1 (list 13 '())) '())
(list (list 1 (list 13 '())))

(list '() '() (cons 1 '()))
(list '() '() (list 1))

(cons "a" (cons (list 1) (list #false '())))
(list "a" (list 1) #false '())

;Reformulate each of the following expressions using only cons or only list.

;; Exercise 184.
;Determine the values of the following expressions:
(list (string=? "a" "b") #false)
(list #false #false)

(list (+ 10 20) (* 10 20) (/ 10 20))
(list 30 200 0.5)

(list "dana" "jane" "mary" "laura")

;; Exercise 185.
;You know about first and rest from BSL, but BSL+ comes with even more selectors than that.
;Determine the values of the following expressions:
(first (list 1 2 3))
1

(rest (list 1 2 3))
(list 2 3)

(second (list 1 2 3))
2

;Find out from the documentation whether third and fourth exist.
;Yes they exist

;; Exercise 186.
;Take a second look at Intermezzo 1: Beginning Student Language,
;the intermezzo that presents BSL and its ways of formulating tests.
;One of the latter is check-satisfied, which determines whether an expression satisfies a certain property.
;Use sorted>? from exercise 145 to reformulate the tests for sort> with check-satisfied.
; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(define (sorted>? list)
  (cond
    [(empty? (rest list)) #true]
    [(and (> (first list) (first (rest list))) 
          (sorted>? (rest list))) #true]
    [else #false]))

(check-satisfied (sort> (list 3 8 12 2 1 85)) sorted>?)

;; Exercise 187.
;Design a program that sorts lists of game players by score:

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points 
;Hint Formulate a function that compares two elements of GamePlayer.

; list-of-players -> list-of-players
; sort a list of players, by player score
(define p1 (make-gp "Mark" 12))
(define p2 (make-gp "Jim" 22))
(define p3 (make-gp "Rob" 49))
(define p4 (make-gp "Vince" 68))

(define p-sorted (list p4 p3 p2 p1))
(define pl1 (list p2 p3 p1 p4))
(define pl2 (list p3 p1))

(check-expect (sort-player pl1) p-sorted)

(define (sort-player p)
  (cond
    [(empty? p) '()]
    [(cons? p) (insert-gp (first p) (sort-player (rest p)))]))


; gp list-of-players -> list-of-players
; inserts gp into sorted list of players l
(check-expect (insert-gp p2 pl2) (list p3 p2 p1))

(define (insert-gp p l)
  (cond
    [(empty? l) (cons p '())]
    [else (if (higher-score? p (first l))
            (cons p l)
            (cons (first l) (insert-gp p (rest l))))]))

; gp gp -> boolean
; compare 2 game-players, true if first has higher score
(check-expect (higher-score? p2 p1) #true)
(check-expect (higher-score? p3 p4) #false)

(define (higher-score? p1 p2)
  (>= (gp-score p1) (gp-score p2)))

;; Exercise 188.
;Design a program that sorts lists of emails by date:
(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time 

;Also develop a program that sorts lists of email messages by name.
;To compare two strings alphabetically, use the string<? primitive.

; list-of-emails -> list-of-emails
; sort list of emails by date
(define m1 (make-email "Rob" 1200 "Hello this is Rob"))
(define m2 (make-email "Vin" 2380 "This is Vin"))
(define m3 (make-email "Jan" 3784 "How are you?"))
(define m4 (make-email "Jack" 5880 "What's up?"))

(define ml1 (list m1 m2 m3 m4))
(define ml2 (list m4 m2 m1 m3))
(define ml3 (list m2 m3))

(check-expect (sort-mail ml2) ml1)

(define (sort-mail ml)
  (cond
    [(empty? ml) '()]
    [(cons? ml) (insert-mail (first ml) (sort-mail (rest ml)))]))


; mail list-of-emails -> list-of-emails
; insert mail into list-of-emails ordered by date
(check-expect (insert-mail m4 ml3) (list m2 m3 m4))

(define (insert-mail m ml)
  (cond
    [(empty? ml) (cons m '())]
    [(earlier-date? m (first ml)) (cons m ml)]
    [else
     (cons (first ml) (insert-mail m (rest ml)))]))


; mail mail -> boolean
; check if mail1 has earlier date than mail2
(check-expect (earlier-date? m1 m2) #true)
(check-expect (earlier-date? m3 m1) #false)

(define (earlier-date? m1 m2)
  (<= (email-date m1) (email-date m2)))

; Also develop a program that sorts lists of email messages by name.
; To compare two strings alphabetically, use the string<? primitive.

; list-of-emails -> list-of-emails
; sort emails by name in alphabetical order
(check-expect (sort-mail-name ml2) (list m4 m3 m1 m2))

(define (sort-mail-name ml)
  (cond
    [(empty? ml) '()]
    [(cons? ml) (insert-mail-name (first ml) (sort-mail-name (rest ml)))]))

(define (insert-mail-name m ml)
    (cond
      [(empty? ml) (cons m '())]
      [(az-name? m (first ml)) (cons m ml)]
      [else
       (cons (first ml) (insert-mail m (rest ml)))]))

(define (az-name? m1 m2)
  (string<? (email-from m1) (email-from m2)))

;; Exercise 189.
;Here is the function search:
; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

; It determines whether some number occurs in a list of numbers.
;The function may have to traverse the entire list to find out
;that the number of interest isn’t contained in the list.
;Develop the function search-sorted, which determines whether a number occurs
;in a sorted list of numbers. The function must take advantage of the fact that the list is sorted.

; Number List-of-numbers -> Boolean
; search number in a sorted list
(check-expect (search-sorted 3 (list 1 2 3)) #true)
(check-expect (search-sorted 4 (list 1 6 9)) #false)
(check-expect (search-sorted 2 '()) #false)
(check-expect (search-sorted 4 (list 1 2 3)) #false)
(check-expect (search-sorted 3 (list 3 4 5)) #true)

(define (search-sorted n alon)
  (cond
   [(empty? alon) #false]
   [(< n (first alon)) #false]
   [else
    (or (= n (first alon)) (search n (rest alon)))]))


;; Exercise 190.
;Design the prefixes function, which consumes a list of 1Strings and produces the list of all prefixes.
;A list p is a prefix of l if p and l are the same up through all items in p.
;For example, (list "a" "b" "c") is a prefix of itself and (list "a" "b" "c" "d").

; list-of-1strings -> list-of-prefix
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a")))

(define (prefixes l)
  (cond
  [(empty? l) '()]
  [else
   (cons l (prefixes (cut-tail l)))]))

(check-expect (cut-tail (list 1 2 3)) (list 1 2))
(check-expect (cut-tail (list 1 2)) (list 1))
(check-expect (cut-tail (list 1)) '())

(define (cut-tail l)
  (cond
  [(empty? l) '()]
  [(empty? (rest l)) '()]
  [else
   (cons (first l) (cut-tail (rest l)))]))



;Design the function suffixes, which consumes a list of 1Strings and produces all suffixes.
;A list s is a suffix of l if p and l are the same from the end, up through all items in s.
;For example, (list "b" "c" "d") is a suffix of itself and (list "a" "b" "c" "d").

; list-of-1strings -> list-of-suffix
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "b" "c") (list "c")))

(define (suffixes l)
  (cond
   [(empty? l) '()]
   [else
    (cons l (suffixes (rest l)))]))



