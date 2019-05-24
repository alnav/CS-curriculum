;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; CONSTANTS:
;; =====================

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 3)
(define MISSILE-HEIGHT (- HEIGHT 30))
(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; DATA DEFINITIONS:
;; =====================

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

;; TANK

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; INVADER

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 6))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -5))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 5)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons (make-invader 100 200 10) (cons (make-invader 150 10 -10) empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self reference: (rest loi) is ListOfInvader

;; MISSILE

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons (make-missile 10 20) (cons (make-missile 300 600) empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self reference: (rest lom) is ListOfMissile


;; GAME-TEST

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; FUNCTIONS:
;; =====================

;; Game -> Game
;; start game by evaluating (main ...)


(define (main game)
  (big-bang game
    (on-key    handle-key)        ; Game KeyEvent -> Game
    (on-tick   next-game)         ; Game -> Game
    (to-draw   render-game)))       ; Game -> Image
;(stop-when invader-bottom?))) ; Game -> Boolean



;; Game KeyEvent -> Game
;; "right" moves tank right
;; "left" moves tank left
;; " " shoots missile

(check-expect (handle-key G2 "right") (make-game (game-invaders G2) (game-missiles G2) (make-tank (tank-x (game-tank G2)) 1)))
(check-expect (handle-key G2 "left") (make-game (game-invaders G2) (game-missiles G2) (make-tank (tank-x (game-tank G2)) (- 1))))
(check-expect (handle-key G2 " ") (make-game (game-invaders G2) (cons (make-missile (tank-x (game-tank G2)) MISSILE-HEIGHT) (game-missiles G2)) (game-tank G2)))
              
;(define (handle-key game event) game) ;stub

(define (handle-key game ke)
  (make-game (game-invaders game)
             (cond [(key=? ke " ")
                    (cons (make-missile
                           (tank-x (game-tank game)) MISSILE-HEIGHT)
                          (game-missiles game))]
                   [else (game-missiles game)])
             (cond [(key=? ke "left")
                    (make-tank
                     (tank-x (game-tank game))
                     (- 1))]
                   [(key=? ke "right")
                    (make-tank
                     (tank-x (game-tank game))
                     1)]
                   [else
                    (game-tank game)])))
                   




;; Game -> Game
;; updates game state


;(define (next-game game) game) ;stub


(define (next-game game)
  (make-game
   (update-invaders (game-invaders game))
   (update-missiles (game-missiles game))
   (update-tank (game-tank game))))


;; ListOfInvader -> ListOfInvader
;; updates invaders in the game
;; !!! need to add bounce off walls
;; !!! need to add, destroy if touches missile
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders (cons I1 empty)) (cons (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                                                      empty))
(check-expect (update-invaders (cons I1 (cons I2 empty))) (cons (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                                                                (cons (make-invader (+ (invader-x I2) (invader-dx I2)) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                                                                      empty)))

;(define (update-invaders game) game)

(define (update-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cond [(> (invader-y (first loi)) HEIGHT)                                             ;invaders below screen
                (update-invaders (rest loi))]
               [(string=? (invader-outside? (first loi)) "out-right")
                (cons (make-invader WIDTH
                                    (+ (invader-y (first loi)) INVADER-Y-SPEED)
                                    (- (invader-dx (first loi))))
                      (update-invaders (rest loi)))]
               [(string=? (invader-outside? (first loi)) "out-left")
                (cons (make-invader 0
                                    (+ (invader-y (first loi)) INVADER-Y-SPEED)
                                    (- (invader-dx (first loi))))
                      (update-invaders (rest loi)))]
               [else (cons (make-invader (+ (invader-x (first loi)) (invader-dx (first loi)))
                                         (+ (invader-y (first loi)) INVADER-Y-SPEED)
                                         (invader-dx (first loi)))
                           (update-invaders (rest loi)))])]))


;; Invader -> boolean
;; check if invader outside right or left margin

(define (invader-outside? inv)
  (cond [(> (invader-x inv) WIDTH)
         "out-right"]
        [(< (invader-x inv) 0)
         "out-left"]
        [else "inside"]))



;; ListOfMissile -> ListOfMissile
;; update-missiles
;; produce filtered and ticked list of drops
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (cons (make-missile 10 20) (cons (make-missile 3 6) empty))) (cons (make-missile 10 (- 20 MISSILE-SPEED))
                                                                                                  (cons (make-missile 3 (- 6 MISSILE-SPEED)) empty)))
(check-expect (update-missiles (cons (make-missile 10 -10) empty)) empty)
(check-expect (update-missiles (cons (make-missile 10 -50) (cons (make-missile 3 6) empty))) (cons (make-missile 3 (- 6 MISSILE-SPEED)) empty))

;(define (update-missiles lom) empty) ; stub


(define (update-missiles lom)
  (cond [(empty? lom) lom]
        [else
         (if (outside? (first lom))
             (update-missiles (rest lom))
             (cons (make-missile
                    (missile-x (first lom))        
                    (- (missile-y (first lom)) MISSILE-SPEED))
                   (update-missiles (rest lom))))]))



;; Missile -> Boolean
;; outside?
;; check if a missile-y is outside boundaries
(check-expect (outside? (make-missile 10 10)) #false)
(check-expect (outside? (make-missile 20 -5)) #true)

;(define (outside? drop) #false)
(define (outside? missile)
  (< (missile-y missile) 0))


;; Tank -> Tank
;; moves tank left or right depending on tank-dx
(check-expect (update-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (update-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1))

(define (update-tank tank)
  (cond [(< (tank-x tank) 0)
         (make-tank 0 (tank-dir tank))]
        [(> (tank-x tank) WIDTH)
         (make-tank WIDTH (tank-dir tank))]
        [else
         (cond [(< (tank-dir tank) 0)
                (make-tank
                 (- (tank-x tank) TANK-SPEED)
                 (tank-dir tank))]
               [(> (tank-dir tank) 0)
                (make-tank
                 (+ (tank-x tank) TANK-SPEED)
                 (tank-dir tank))])]))
                    




;; Game -> Image
;; renders game
;(define (render-game game) (square 1 "solid" "white")) ;stub


(define (render-game game)
  (place-images
   (join-img (game-invaders game)
             (game-missiles game))
   (join-posns (game-invaders game)
               (game-missiles game)
               (game-tank game))
   BACKGROUND))




;; ListOfInvader -> ListOfImage
;; Produces a ListOfImage of same size as ListOfInvader
(check-expect (list-img-inv empty) empty)
(check-expect (list-img-inv (cons I1 empty)) (cons INVADER empty))
(check-expect (list-img-inv (cons I1 (cons I2 empty))) (cons INVADER
                                                             (cons INVADER empty)))


(define (list-img-inv loi)
  (cond [(empty? loi) empty]
        [else
         (cons INVADER 
               (list-img-inv (rest loi)))]))


;; ListOfMissile -> ListOfImage
;; Produces a ListOfImage of same size as ListOfMissile
(check-expect (list-img-mis empty) empty)
(check-expect (list-img-mis (cons M1 empty)) (cons MISSILE empty))
(check-expect (list-img-mis (cons M1 (cons M2 empty))) (cons MISSILE
                                                             (cons MISSILE empty)))


(define (list-img-mis lom)
  (cond [(empty? lom) empty]
        [else
         (cons MISSILE 
               (list-img-mis (rest lom)))]))

;; ListOfInvader ListOfMissile -> ListOfImage
;; appends all lists of images into one, adding tank at the end
(check-expect (join-img (cons I1 (cons I2 empty)) (cons M1 (cons M2 empty))) (append (list-img-inv (cons I1 (cons I2 empty)))
                                                                                     (list-img-mis (cons M1 (cons M2 empty)))
                                                                                     (cons TANK empty)))

(define (join-img loi lom)
  (append (list-img-inv loi)
          (list-img-mis lom)
          (cons TANK empty)))



;; ListOfInvader -> ListOfPosn
;; produces a list of position-x and position-y from ListOfInvader
(check-expect (list-posn-inv (cons I1 empty)) (cons (make-posn (invader-x I1) (invader-y I1)) empty))
(check-expect (list-posn-inv (cons I1 (cons I2 empty))) (cons (make-posn (invader-x I1) (invader-y I1))
                                                              (cons (make-posn (invader-x I2) (invader-y I2)) empty)))


(define (list-posn-inv loi)
  (cond [(empty? loi) empty]
        [else
         (cons (make-posn (invader-x (first loi))
                          (invader-y (first loi)))
               (list-posn-inv (rest loi)))]))


;; ListOfMissiles -> ListOfMissiles
;; produces a list of position-x and position-y from ListOfMissiles
(check-expect (list-posn-mis (cons M1 empty)) (cons (make-posn (missile-x M1) (missile-y M1)) empty))
(check-expect (list-posn-mis (cons M1 (cons M2 empty))) (cons (make-posn (missile-x M1) (missile-y M1)) (cons (make-posn (missile-x M2) (missile-y M2)) empty)))


(define (list-posn-mis lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (missile-x (first lom))
                          (missile-y (first lom)))
               (list-posn-mis (rest lom)))]))


;; ListOfInvader ListOfMissile Tank -> ListOfPosn
;; appends all lists of posns into one, adding tank at the end
(check-expect (join-posns (cons I1 (cons I2 empty)) (cons M1 (cons M2 empty)) (make-tank 100 1)) (append (list-posn-inv (cons I1 (cons I2 empty))) (list-posn-mis (cons M1 (cons M2 empty))) (cons (make-posn 100 480) empty)))

(define (join-posns loi lom tank)
  (append (list-posn-inv loi)
          (list-posn-mis lom)
          (cons (make-posn (tank-x tank) (- HEIGHT 20)) empty)))


;; Game -> Boolean
;; Stop game when invaders reaches bottom of screen
;; !!!

(define (invader-bottom? game) #true) ;stub



  























