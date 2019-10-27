;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 199-2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)
(require 2htdp/image)


(define ITUNES-LOCATION "itunes.xml")
(define I (read-itunes-as-tracks ITUNES-LOCATION))
 
; An LTracks is one of:
; – '()
; – (cons Track LTracks)


 
;(define-struct track
;  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played

 
;(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).


; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track for legitimate inputs
; otherwise it produces #false
#;
(define (create-track name artist album t track# added play# played)
  (make-track name artist album t track# added play# played))
 
; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date for legitimate inputs 
; otherwise it produces #false
#;
(define (create-date y mo day h m s)
  (make-date y mo day h m s))
 
; String -> LTracks
; creates a list-of-tracks representation from the
; text in file-name (an XML export from iTunes)
#;
(define (read-itunes-as-tracks file-name)
  ...)

; ----------------
;; Exercise 199.
; ----------------
;While the important data definitions are already provided, the first step of the
;design recipe is still incomplete. Make up examples of Dates, Tracks, and LTracks.
;These examples come in handy for the following exercises as inputs.
(define d1 (create-date 2018 03 23 10 05 44))
(define d2 (create-date 1989 04 12 04 09 55))

(define t1 (create-track "You give love a bad name" "Bon Jovi" "alb1" 2392302 2 d1 9 d2))
(define t2 (create-track "Party all night" "Jaguars" "alb2" 1892307 1 d2 11 d1))

(define Lt1 (list t1))
(define Lt2 (list t1 t2))

; ----------------
;; Exercise 200.
; ----------------
;Design the function total-time, which consumes an element of LTracks and produces
;the total amount of play time. Once the program is done, compute the total play time of your iTunes collection.

; LTracks -> Number
; interp. computes total duration of tracks in a LTrack
(check-expect (total-time Lt1) 2392302)
(check-expect (total-time Lt2) (+ 2392302 1892307))

(define (total-time LTracks)
  (cond
    [(empty? LTracks) 0]
    [else
     (+ (track-time (first LTracks)) (total-time (rest LTracks)))]))

; total time
; (total-time (read-itunes-as-tracks ITUNES-LOCATION))



; ----------------
;; Exercise 201.
; ----------------
;Design select-all-album-titles.
;The function consumes an LTracks and produces the list of album titles as a List-of-strings.

; LTracks -> List-of-strings
; interp. produces a list of album titles from a LTracks
(check-expect (select-all-album-titles Lt1) (list "alb1"))
(check-expect (select-all-album-titles Lt2) (list "alb1" "alb2"))

(define (select-all-album-titles l)
  (cond
    [(empty? l) '()]
    [else
     (cons (track-album (first l)) (select-all-album-titles (rest l)))]))

; Also design the function create-set.
; It consumes a List-of-strings and constructs one that contains every String from the given list exactly once.
;Hint If String s is at the front of the given list and occurs in the rest of the list, too, create-set does not keep s.

; List-of-strings -> List-of-strings
; create a set from a List-of-strings
(check-expect (create-set (list "John" "Mark" "John")) (list "Mark" "John"))
(check-expect (create-set (list "Louise" "Mark" "John" "Mark" "Rob" "Mark")) (list "Louise" "John" "Rob" "Mark"))

(define (create-set l)
  (cond
    [(empty? l) '()]
    [(member? (first l) (rest l))
         (create-set (rest l))]
    [else
     (cons (first l) (create-set (rest l)))]))

; Finally design select-album-titles/unique, which consumes an LTracks and produces a list of unique album titles.
;Use this function to determine all album titles in your iTunes collection
;and also find out how many distinct albumsit contains.

; LTracks -> List-of-strings
; intepr. creates a set of album titles from a LTracks
(check-expect (select-album-titles/unique (list t1 t2 t1)) (list "alb2" "alb1"))

(define (select-album-titles/unique l)
  (create-set (select-all-album-titles l)))

;; how many unique album titles:
; (length (select-album-titles/unique (read-itunes-as-tracks ITUNES-LOCATION)))

; ----------------
;; Exercise 202.
; ----------------
;Design select-album. The function consumes the title of an album and an LTracks.
;It extracts from the latter the list of tracks that belong to the given album.

; String LTracks -> List-of-Strings
; intepr. receives an album title and a LTracks, output is list of tracks in given album
(check-expect (select-album "alb2" Lt2) (list t2))

(define (select-album a l)
  (cond
    [(empty? l) '()]
    [(string=? a (track-album (first l)))
     (cons (first l) (select-album a (rest l)))]
    [else
     (select-album a (rest l))]))

; ----------------
;; Exercise 203.
; ----------------
;Design select-album-date.
;The function consumes the title of an album, a date, and an LTracks.
;It extracts from the latter the list of tracks that belong to the given album
;and have been played after the given date.
;Hint You must design a function that consumes two Dates and determines whether the first occurs before the second.
(check-expect (select-album-date "alb1" d1 Lt2) (list t1))

(define (select-album-date a d l)
  (cond
    [(empty? l) '()]
    [(and (earlier? (track-played (first l)) d)
          (string=? (track-album (first l)) a))
     (cons (first l) (select-album-date a d (rest l)))]
    [else
     (select-album-date a d (rest l))]))


; Date Date -> Boolean
; returns true if first date occurs before second
(check-expect (earlier? d2 d1) #true)
(check-expect (earlier? d1 d2) #false)

(define (earlier? d-1 d-2)
  (< (date->seconds d-1) (date->seconds d-2)))

; Date -> Seconds
; converts a date to seconds
(define (date->seconds d)
  (+ (* (date-year d) 60 60 24 365)
     (* (date-month d) 60 60 24 12)
     (* (date-day d) 60 60 24)
     (* (date-hour d) 60 60)
     (* (date-minute d) 60)
     (date-second d)))

; ----------------
;; Exercise 204. 
; ----------------
;Design select-albums.
;The function consumes an element of LTracks.
;It produces a list of LTracks, one per album.
;Each album is uniquely identified by its title and shows up in the result only once.
;Hints (1) You want to use some of the solutions of the preceding exercises.
;(2) The function that groups consumes two lists: the list of album titles and the list of tracks;
;it considers the latter as atomic until it is handed over to an auxiliary function. See exercise 196.

; LTracks -> list-of-LTracks
; interpr. returns a list of albums from an LTracks
(define-struct alb-list [album LTracks])
; (make-alb-list String LTracks)

(define (select-albums-help alb-list Tlist)
  (cond
    [(empty? alb-list) '()]
    [else
     (cons (make-alb-list (first alb-list) (select-album (first alb-list) Tlist))
           (select-albums-help (rest alb-list) Tlist))]))

(define (select-albums l)
  (select-albums-help (select-album-titles/unique l) l))


; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
 
; String -> LLists
; creates a list of lists representation for all tracks in 
; file-name, which must be an XML export from iTunes 

(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; ----------------
;; Exercise 205.
; ----------------
;Develop examples of LAssoc and LLists, that is, the list representation of tracks and lists of such tracks.
(define A1 (list "Name" "Money"))
(define A2 (list "Artist" "Pink Floyd"))
(define A3 (list "Album" "Dark Side of the Moon"))
(define A4 (list "Rock" #true))

(define LA1 (list A1 A2))
(define LA2 (list A2 A3))

(define LL1 (list LA1 LA2))

; ----------------
;; Exercise 206. 
; ----------------
;Design the function find-association.
;It consumes three arguments: a String called key, an LAssoc, and an element of Any called default.
;It produces the first Association whose first item is equal to key, or default if there is no such Association.

; String LAssoc Any -> Association
(check-expect (find-association "Name" LA1 "Default") A1)

(define (find-association n l d)
  (cond
    [(empty? l) d]
    [(string=? n (first (first l))) (first l)]
    [else (find-association n (rest l) d)]))

; ----------------
;; Exercise 207.
; ----------------
;Design total-time/list, which consumes an LLists and produces the total amount of play time.
;Hint Solve exercise 206 first.

;Once you have completed the design, compute the total play time of your iTunes collection.
;Compare this result with the time that the total-time function from exercise 200 computes.
;Why is there a difference? - total-time/list also computes time of other files which are not tracks

; LList -> Natural
(define (total-time/list l)
  (cond
    [(empty? l) 0]
    [else
     (+ (second (find-association "Total Time" (first l) 0))
        (total-time/list (rest l)))]))

; ----------------
;; Exercise 208.
; ----------------
;Design boolean-attributes.
;The function consumes an LLists and produces the Strings that are associated with a Boolean attribute.
;Hint Use create-set from exercise 201.

;Once you are done, determine how many Boolean-valued attributes your iTunes library employs for its tracks.
;Do they make sense?
(define (boolean-attributes l)
  (create-set (clean-list-bool (list-bool l))))
  

; Association -> boolean
; true if the association has a boolean as second value
(check-expect (assoc-bool? A4) #true)
(check-expect (assoc-bool? A1) #false)

(define (assoc-bool? a)
  (boolean? (second a)))

;LList -> LAssoc
; interp. creates a LAssoc who contains boolean values, from a LList
(define (list-bool-ac l)
  (cond
    [(empty? l) '()]
    [(assoc-bool? (first l))
     (cons (first l) (list-bool-ac (rest l)))]
    [else
     (list-bool-ac (rest l))]))


(define (list-bool l)
  (cond
    [(empty? l) '()]
    [else
     (append (list-bool-ac (first l)) (list-bool (rest l)))]))

(define (clean-list-bool l)
  (cond
    [(empty? l) '()]
    [else
     (cons (first (first l)) (clean-list-bool (rest l)))]))

