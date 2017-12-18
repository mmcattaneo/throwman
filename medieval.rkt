;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname medieval) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; -- THROWMAN: the game --                                             ;;;;;
;;;;; -- MEDIEVAL SCENARIO --                                              ;;;;;
;;;;; Developed by: Cattaneo Marco, Ferrara Salvatore, Willi Patrizia      ;;;;;
;;;;; Final project - Programming Fundamentals 1, Prof. N. Nystrom         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Importing packages required

(require 2htdp/image)      ; For images and shapes.
(require 2htdp/universe)   ; For the Big Bang.
(require racket/base)      ; For providing functions to the main menu.


; Providing the big bang to the main menu.

(provide medieval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definining the struct that keeps track of the game status.

; An arrow is Struct (make-arrow ...) where:
; - mx1: is the x position of the mouse when the "button down" event happens.
; - my1: is the y position of the mouse when the "button down" event happens.
; - mx2: is the x position of the mouse when the "drag" event happens.
; - my2: is the y position of the mouse when the "drag" event happens.
; - arrowPosX: is the x coordinate where the arrow should be drawn.
; - arrowPosX: is the y coordinate where the arrow should be drawn.
; - rot: is the rotation of the arrow on its pinhole (Int).
; - turn: is a Boolean, when true is player one's turn.
; - power: is an Int [0,100] and defines the speed of the object being shot.
; - life1: is an Int [0,100] that keeps track of the life of the first player.
; - life2: is an Int [0,100] that keeps track of the life of the second player.
; - move: is a Boolean that describes when the arrow should move according to
;         its trajectory.
; - printMessage?: is a Boolean that when true triggers function that returns
;                  the message with the winner of the match.
; - shouldStop?: is a Boolean that defines when the big bang should stop.
; - wind: is an random value between [-10, 10] that defines the intensity of the
;         wind. This value starts at zero and changes every turn.
; - out: is a Boolean that becomes true when the projectile is out of scene
;        (left, right or bottom).

(define-struct arrow
  [mx1 my1 mx2 my2
       arrowPosX arrowPosY
       rot
       turn line
       angle power
       life1 life2
       move printMessage?
       shouldStop?
       wind
       out])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Building the scenario of the game.

; Defining the WIDTH of the scene in pixels.
(define WIDTH 1400)


; The constant SCENARIO is constant that represent the image of the scenario.
(define SCENARIO
  (bitmap "medieval.jpg"))


; initialState is a (make-arrow) with the initial game scenario.
(define initialState
  (make-arrow 0 0 0 0
              130 350
              0
              #true
              #false
              0 0
              100 100
              #false #false
              #false
              0
              #true))


; ARROW is a constant that defines the picture of the arrow.
(define ARROW
  (put-pinhole
   0 47
   (overlay/xy
    (beside
     ; building the tail of the arrow.
     (rotate 270 (triangle 6 "solid" "white"))
     (rotate 270 (triangle 6 "solid" "white")))
    0 -0.25
    (overlay/xy
     ; building the central body of the arrow
     (rectangle 45 1.5 "solid" "white")
     37.5 -2.2
     ; building the head of the arrow.
     (rotate 270 (isosceles-triangle 13 25 "solid" "white"))))))


; TURN is a yellow triangle used to indicate the turn.
(define TURN
  (rotate 180 (triangle 25 "solid" "yellow")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following part of the code defines the conditions that indicates when a
; player has been hit by the object.

; Signature: Int -> Boolean
; Interpretation: for a given x returns if this x is occupied by a player.
(define (hasHitP2?X x)
  (cond[(and
         (< x (- WIDTH 50))
         (> x (- WIDTH 130)))
        #true]
       [else #false]))

(check-expect (hasHitP2?X 1300) #true)
(check-expect (hasHitP2?X 200) #false)
(check-expect (hasHitP2?X 50) #false)

; Signature: Int -> Boolean
; Interpretation: for a given y returns if this y is occupied by the player two.
(define (hasHitP2?Y y)
  (cond[(and
         (> y -10)
         (< y 140))
        #true]
       [else #false]))

(check-expect (hasHitP2?Y 100) #true)
(check-expect (hasHitP2?Y -20) #false)
(check-expect (hasHitP2?Y 50) #true)


; Signature: Int -> Boolean
; Interpretation: checks whether the arrow is permanently out of the scene on
; the x axis.
(define (isOut? x)
  (cond[(or
         (< x 0)
         (> x WIDTH))
        #true]
       [else #false]))

(check-expect (isOut? -10) #true)
(check-expect (isOut? 500) #false)
(check-expect (isOut? 1200) #false)

; Signature: Int -> Boolean
; Interpretation: checks whether the arrow has hit ground.
(define (hasHitGround? x)
  (cond[(or
         (> x 370)
         (< x 0))
        #true]
       [else #false]))

(check-expect (hasHitGround? 0) #false)
(check-expect (hasHitGround? 300) #false)
(check-expect (hasHitGround? -10) #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Defining life and wind bars.


; Signature: Int -> Image
; Interpretation: for a given life (Int [0,100]) returns a in image of
; the life bar.
(define (HP-line x)
  (overlay/align "center" "center"
                 (overlay/align "center" "center"
                                (text (string-append
                                       (number->string x)
                                       "%")
                                      12 "white")
                                (overlay/align "left" "middle"
                                               (cond
                                                 [(> x 0)
                                                  (rectangle
                                                   (* 2 x)
                                                   15
                                                   "solid"
                                                   "forestgreen")]
                                                 [else
                                                  (rectangle
                                                   0 15 "solid" "forestgreen")])
                                               (rectangle
                                                200 15 "solid" "crimson")))
                 (rectangle 202 18 "outline" "white")))


; Signature: Int -> Image
; Interpretation: for a given wind (Int [-10,10]) returns an image of the
; wind bar.
(define (WIND-line arrow)
  (cond [(<= (arrow-wind arrow) 0)
         (overlay/offset
          (rectangle (* 100 (abs (arrow-wind arrow))) 10 "solid" "black")
          (- 0 (/ (* 100 (arrow-wind arrow)) 2)) 0
          (rectangle 200 10 "outline" "black"))]
        
        [(> (arrow-wind arrow) 0)
         (overlay/offset
          (rectangle (* 100 (arrow-wind arrow)) 10 "solid" "black")
          (- 0 (/ (* 100 (arrow-wind arrow)) 2)) 0
          (rectangle 200 10 "outline" "black"))]))
         

  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Function that draws the scene.

; Signature: Struct -> Image
; Interpretation: for a given struct returns the new world state.

(define (draw-scene arrow)

  ; Condition that draws the line and overlays the various strings
  ; indicating the angle and speed of the arrow.
  (cond [(and
          (eq? (arrow-move arrow) #false)
          (eq? (arrow-line arrow) #true))
         (overlay/xy
          TURN
          (cond [(eq? (arrow-out arrow) #true)
                 -100]
                [else
                 (- (- 0 WIDTH) -104)]) 
          -245
          (overlay/xy
           (WIND-line arrow)
           -600 -66
           (overlay/xy
            (HP-line (arrow-life1 arrow))
            -130 -57
            (overlay/xy
             (HP-line (arrow-life2 arrow))
             (- (- WIDTH 335)) -57
             (overlay/align "center" "bottom"
                            (text
                             (string-append "Angle: "
                                            (number->string
                                             (floor (arrow-angle arrow)))
                                            "°"
                                            " - Speed: "
                                            (number->string
                                             (arrow-power arrow)))
                             13 "white")
                            (add-line
                             SCENARIO
                             (arrow-mx1 arrow)
                             (arrow-my1 arrow)
                             (arrow-mx2 arrow)
                             (arrow-my2 arrow)
                             (pen "white" 4 "solid" "round" "bevel")))))))]

        ; Condition that draws the moving arrow when the
        ; it's first player's turn.
        [(and
          (eq? (arrow-line arrow) #false)
          (eq? (arrow-move arrow) #true)
          (eq? (arrow-turn arrow) #true))
         
         (place-image
          (rotate (arrow-rot arrow) ARROW)
          (arrow-arrowPosX arrow)
          (- 400 (arrow-arrowPosY arrow))
          (overlay/xy
           TURN
           (cond [(eq? (arrow-out arrow) #true)
                  -100]
                 [else
                  (- (- 0 WIDTH) -104)]) 
           -245
           (overlay/xy
            (HP-line (arrow-life1 arrow))
            -130 -57                           
            (overlay/xy
             (WIND-line arrow)
             -600 -66
             (overlay/xy
              (HP-line (arrow-life2 arrow))
              (- (- WIDTH 335)) -57
              SCENARIO)))))]

        ; Condition that draws the moving arrow when the
        ; it's second player's turn.
        [(and
          (eq? (arrow-line arrow) #false)
          (eq? (arrow-move arrow) #true)
          (eq? (arrow-turn arrow) #false))
         
         (place-image
          (rotate  (- 180 (arrow-rot arrow)) ARROW)
          (- WIDTH (arrow-arrowPosX arrow))
          (- 400 (arrow-arrowPosY arrow))

          (overlay/xy
           TURN
           (cond [(eq? (arrow-out arrow) #true)
                  -100]

                 [else
                  (- (- 0 WIDTH) -104)]) 
           -245
           (overlay/xy
            (WIND-line arrow)
            -600 -66
            (overlay/xy
             (HP-line (arrow-life1 arrow))
             -130 -57          
             (overlay/xy
              (HP-line (arrow-life2 arrow))
              (- (- WIDTH 335)) -57
              SCENARIO)))))]

        ; Condition that prints the message "Player 2 WINS!" when
        ; the life of the first player is lower or equal to zero.
        [(and
          (<= (arrow-life1 arrow) 0)
          (eq? (arrow-printMessage? arrow) #true))
         
         (overlay/align "center" "center"
                        (text
                         "Player 2 WINS!"
                         40 "white")
                        (overlay/align "center" "center"
                                       (rectangle 400 100 "solid" "black")
                                       SCENARIO))]

        ; Condition that prints the message "Player 1 WINS!" when
        ; the life of the second player is lower or equal to zero.        
        [(and
          (<= (arrow-life2 arrow) 0)
          (eq? (arrow-printMessage? arrow) #true))
         
         (overlay/align "center" "center"
                        (text
                         "Player 1 WINS!"
                         40 "white")
                        (overlay/align "center" "center"
                                       (rectangle 400 100 "solid" "black")
                                       SCENARIO))]

        ; This condition draws the scenario when no line is being drawn and
        ; the arrow is not moving.
        ; In this case the TURN indicator, the wind and life bars are drawn
        ; over the image.
        [(and
          (eq? (arrow-move arrow) #false)
          (eq? (arrow-line arrow) #false))
         (overlay/xy
          TURN
          (cond [(eq? (arrow-out arrow) #true)
                 -100]
                [else
                 (- (- 0 WIDTH) -104)]) 
          -245
          (overlay/xy
           (WIND-line arrow)
           -600 -66
           (overlay/xy
            (HP-line (arrow-life1 arrow))
            -130 -57
            (overlay/xy
             (HP-line (arrow-life2 arrow))
             (- (- WIDTH 335)) -57
             SCENARIO))))]
        
        ; In the case none of the previous conditions is met,
        ; the TURN indicator, the wind and life bars are drawn over the image.
        [else
         (overlay/xy
          TURN
          (cond [(eq? (arrow-out arrow) #true)
                 -100]
                [else
                 (- (- 0 WIDTH) -104)]) 
          -245
          (overlay/xy
           (WIND-line arrow)
           -600 -66
           (overlay/xy
            (HP-line (arrow-life1 arrow))
            -130 -57
            (overlay/xy
             (HP-line (arrow-life2 arrow))
             (- (- WIDTH 335)) -57
             SCENARIO))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; On tick function

; Signature: Structure -> Structure
; Interpretation: updates a given structure every tick according to the various
; changes in the game state.

(define (move-arrow arrow)
  ; When there is no message printed and no line being drawn and the
  ; arrow is moving on the scene and it's player one's turn.
  (cond[(and
         (eq? (arrow-printMessage? arrow) #false)
         (eq? (arrow-line arrow) #false)
         (eq? (arrow-move arrow) #true)
         (eq? (arrow-turn arrow) #true))
        
        (make-arrow
         (arrow-mx1 arrow)
         (arrow-my1 arrow)
         (arrow-mx2 arrow)
         (arrow-my2 arrow)

         ; The x position of the arrow is updated only if the rival player
         ; is not hit. When the rival player is hit the arrow stops.
         ; The x position is updated based on the power defined by the user
         ; with the mouse interaction.
         (cond[(and
                (hasHitP2?X (arrow-arrowPosX arrow))
                (hasHitP2?Y (arrow-arrowPosY arrow)))
               (arrow-arrowPosX arrow)]
              [else
               (+
                (speed-x (/ (arrow-power arrow) 3) (arrow-angle arrow))
                (arrow-arrowPosX arrow))])

         ; The y position of the arrow is updated only if the rival player
         ; is not hit. When the rival player is hit the arrow stops.
         ; The y position is updated according to the external function
         ; trajectory that computes the exact parabola followed by the arrow
         ; during its fly. The trajectory is also influenced by the wind.
         (cond [(and
                 (hasHitP2?Y (arrow-arrowPosY arrow))
                 (hasHitP2?X (arrow-arrowPosX arrow)))
                (arrow-arrowPosY arrow)]

               [else
                (cond [(>= (arrow-wind arrow) 0)
                       (trajectory
                        (velocity (+
                                   (arrow-power arrow)
                                   (* (/
                                       (arrow-wind arrow) 10)
                                      (arrow-power arrow)))
                                  (arrow-angle arrow))
                        100 180 (arrow-angle arrow) (arrow-arrowPosX arrow))]
                      
                      [else
                       (trajectory
                        (velocity (+
                                   (arrow-power arrow)
                                   (* (/
                                       (arrow-wind arrow) 10)
                                      (arrow-power arrow)))
                                  (arrow-angle arrow))
                        100 180 (arrow-angle arrow) (arrow-arrowPosX arrow))])])

         ; The rotation of the arrow is updated while moving.
         (- (arrow-rot arrow) (* (rot arrow) 400))
       
         (arrow-turn arrow) 
         (arrow-line arrow)
         (arrow-angle arrow)
         (arrow-power arrow)
         (arrow-life1 arrow)

         ; Defining the damage inflicted by the arrow on the player.
         
         ; When the head is hit 50 of 100 life points are deducted by the
         ; current life of the player.
         (cond[(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow))
                (and
                 (<= (arrow-arrowPosY arrow) 140)
                 (> (arrow-arrowPosY arrow) 115)))
               (- (arrow-life2 arrow) 50)] 

              ; When the chest is hit 30 of 100 life points are deducted by the
              ; current life of the player.
              [(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow)) 
                (and
                 (<= (arrow-arrowPosY arrow) 115)
                 (> (arrow-arrowPosY arrow) 55)))
               (- (arrow-life2 arrow) 30)]

              ; When the lower body is hit, 20 of 100 life points are deducted
              ; by the current life of the player.
              [(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow))
                (and
                 (<= (arrow-arrowPosY arrow) 55)
                 (>= (arrow-arrowPosY arrow) -10)))
               (- (arrow-life2 arrow) 20)]

              ; In other cases the life is not changed.
              [else
               (arrow-life2 arrow)])
         
         ; When the player is hit by the arrow the arrow stops.
         ; If no player is hit the arrow keeps moving until it is out of
         ; the scene.
         (cond[(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow)))
               #false]
              [else
               #true])
         
         #false
         #false
         (arrow-wind arrow)
         (arrow-out arrow))]

       ;;;;;;;;

       ; When there is no message printed and no line being drawn and the
       ; arrow is moving on the scene and it's player two's turn.
       [(and
         (eq? (arrow-line arrow) #false)
         (eq? (arrow-printMessage? arrow) #false)
         (eq? (arrow-move arrow) #true)
         (eq? (arrow-turn arrow) #false))
        
        (make-arrow
         (arrow-mx1 arrow)
         (arrow-my1 arrow)
         (arrow-mx2 arrow)
         (arrow-my2 arrow)

         ; The x position of the arrow is updated only if the rival player
         ; is not hit. When the rival player is hit the arrow stops.
         ; The x position is updated based on the power defined by the user
         ; with the mouse interaction.

         (cond [(and
                 (hasHitP2?X (arrow-arrowPosX arrow))
                 (hasHitP2?Y (arrow-arrowPosY arrow)))
                (arrow-arrowPosX arrow)]
               [else
                (+
                 (speed-x (/ (arrow-power arrow) 3)
                          (arrow-angle arrow))
                 (arrow-arrowPosX arrow))])

         ; The y position of the arrow is updated only if the rival player
         ; is not hit. When the rival player is hit the arrow stops.
         ; The y position is updated according to the external function
         ; trajectory that computes the exact parabola followed by the arrow
         ; during its fly. The trajectory is also influenced by the wind.
         (cond [(and
                 (hasHitP2?Y (arrow-arrowPosY arrow))
                 (hasHitP2?X (arrow-arrowPosX arrow)))
                (arrow-arrowPosY arrow)]
               [else
                (cond [(>= (arrow-wind arrow) 0)
                       (trajectory
                        (velocity (- (arrow-power arrow)
                                     (* (/ (arrow-wind arrow) 10)(arrow-power arrow)))
                                  (arrow-angle arrow))
                        100 180 (arrow-angle arrow) (arrow-arrowPosX arrow))]
                
                      [else
                       (trajectory
                        (velocity (- (arrow-power arrow)
                                     (* (/ (arrow-wind arrow) 10)(arrow-power arrow)))
                                  (arrow-angle arrow))
                        100 180 (arrow-angle arrow) (arrow-arrowPosX arrow))])])

         ; The rotation of the arrow is updated while moving.
         (-  (arrow-rot arrow)  (* (rot arrow) 400))
            
         (arrow-turn arrow)
         (arrow-line arrow)
         (arrow-angle arrow)
         (arrow-power arrow)

         ; Defining the damage inflicted by the arrow on the player.
         
         ; When the head is hit 50 of 100 life points are deducted by the
         ; current life of the player.
         (cond[(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow))
                (and
                 (<= (arrow-arrowPosY arrow) 140)
                 (> (arrow-arrowPosY arrow) 115)))
               (- (arrow-life1 arrow) 50)]

              ; When the chest is hit 30 of 100 life points are deducted by the
              ; current life of the player.
              [(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow)) 
                (and
                 (<= (arrow-arrowPosY arrow) 115)
                 (> (arrow-arrowPosY arrow) 55)))
               (- (arrow-life1 arrow) 30)]

              ; When the lower body is hit, 20 of 100 life points are deducted
              ; by the current life of the player.
              [(and
                (hasHitP2?Y (arrow-arrowPosY arrow))
                (hasHitP2?X (arrow-arrowPosX arrow))
                (and
                 (<= (arrow-arrowPosY arrow) 55)
                 (>= (arrow-arrowPosY arrow) -10)))
               (- (arrow-life1 arrow) 20)]
              
              ; In other cases the life is not changed.
              [else
               (arrow-life1 arrow)])
         
         (arrow-life2 arrow)
         
         ; When the player is hit by the arrow the arrow stops.
         ; If no player is hit the arrow keeps moving until it is out of
         ; the scene.
         (cond [(and
                 (hasHitP2?Y (arrow-arrowPosY arrow))
                 (hasHitP2?X (arrow-arrowPosX arrow)))
                #false]
               [else
                #true])

         #false
         #false
         (arrow-wind arrow)
         (arrow-out arrow))]


       ;;;;;;;

       ; When it's first player's turn and the arrow
       ; goes out of the scene without hitting any player.

       [(and
         (eq? (arrow-turn arrow) #true)
         (eq? (arrow-printMessage? arrow) #false)
         (or
          (isOut? (arrow-arrowPosX arrow))
          (hasHitGround? (arrow-arrowPosY arrow))
          (< (arrow-arrowPosY arrow) 0)
          (and
           (hasHitP2?X (arrow-arrowPosX arrow))
           (hasHitP2?Y (arrow-arrowPosY arrow)))))

        ; The structure is redefined, turn changed and
        ; the starting position of the arrow is also restored.
        ; A new random value of wind is created.
        (make-arrow
         (arrow-mx1 arrow)
         (arrow-my1 arrow)
         (arrow-mx2 arrow)
         (arrow-my2 arrow)
         130
         350
         180
         #false
         (arrow-line arrow)
         (arrow-angle arrow)
         (arrow-power arrow)
         (arrow-life1 arrow)
         (arrow-life2 arrow)
         #false
         #false
         #false
         (/ (* 10 (random -10 10)) 100)
         (arrow-out arrow))]


       ;;;;;;
       
       ; When it's first player's turn and the arrow
       ; goes out of the scene without hitting any player.
       [(and
         (eq? (arrow-turn arrow) #false)
         (eq? (arrow-printMessage? arrow) #false)
         (or
          (isOut? (arrow-arrowPosX arrow))
          (hasHitGround? (arrow-arrowPosY arrow))
          (< (arrow-arrowPosY arrow) 0)
          (and
           (hasHitP2?X (arrow-arrowPosX arrow))
           (hasHitP2?Y (arrow-arrowPosY arrow)))))

        
        ; The structure is redefined, turn changed and
        ; the starting position of the arrow is also restored.
        ; A new random value of wind is created.        
        (make-arrow
         (arrow-mx1 arrow)
         (arrow-my1 arrow)
         (arrow-mx2 arrow)
         (arrow-my2 arrow)
         130
         350
         0
         #true
         (arrow-line arrow)
         (arrow-angle arrow)
         (arrow-power arrow)
         (arrow-life1 arrow)
         (arrow-life2 arrow)
         #false
         #false
         #false
         (/ (* 10 (random -10 10)) 100)
         (arrow-out arrow))]


       ; When one of the two players' life is equal or lower than zero a
       ; message with the winner is displayed.
       [(and
         (eq? (arrow-printMessage? arrow) #false)
         (or
          (eq? (arrow-turn arrow) #false)
          (eq? (arrow-turn arrow) #true))
         (or
          (<= (arrow-life1 arrow) 0)
          (<= (arrow-life2 arrow) 0)))
        (make-arrow
         (arrow-mx1 arrow)
         (arrow-my1 arrow)
         (arrow-mx2 arrow)
         (arrow-my2 arrow)
         130
         350
         0
         #true
         #false
         (arrow-angle arrow)
         (arrow-power arrow)
         (arrow-life1 arrow)
         (arrow-life2 arrow)
         #false
         #true
         #false
         (arrow-wind arrow)
         (arrow-out arrow))]
      
       
       ; In any other case the unmodified struct is given to the
       ; big bang.
       [else arrow]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: Num Num -> Num
; Interpretation: given a power and an angle returns the y vector of the
; initial velocity.

(define (speed-y power alpha)
  (* power (sin (degrees->radiant alpha))))

(check-within (speed-y 100 15) 25.881895468215806 0.001)
(check-within (speed-y 100 25) 42.26181203415041 0.001)


; Signature: Num Num -> Num
; Interpretation: given a power and an angle returns the x vector of the
; initial velocity.

(define (speed-x power alpha)
  (* power (cos (degrees->radiant alpha))))

(check-within (speed-x 100 15) 96.59258505171269 0.001)
(check-within (speed-x 100 25) 90.63078529721642 0.001)


; Signature: Int Int -> Num
; Interpretation: given two x and y coordinates, returns the distance between
; the two.
(define (distance-two-points x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(check-within (distance-two-points 1 1 2 2) 1.4142135623730951 0.001)
(check-within (distance-two-points 1 2 6 8) 7.810249675906654 0.001)


; Signature: Num Num Num -> Num
; Interpretation: given the length of the three sides of a triangle,
; returns the angle formed by the triangle.
; This will be used to compute the initial angle of the shot.
(define (alpha a b c)
  (cond [(> c 0)
         (* -1 (acos ( / (- (* c c) (* b b) (* a a) ) (* -2 a b))) 57.2958)]
        [else
         (* (acos ( / (- (* c c) (* b b) (* a a) ) (* -2 a b))) 57.2958)]))

(check-within (alpha 2 4 6) -180.00006436155007 0.001)


; Defining the gravity.
(define G 5.81)

; Defining the initial velocity as a product of the power chosen by
; the user.
; Signature: Structure -> Num
; Interpretation: given a structure, returns the initial velocity.
(define (initial-velocity arrow)
  (* (/ (arrow-power arrow) 100) (* WIDTH 10)))


; Signature: Num -> Num
; Interpretation: given an angle in degrees, returns the angle in
; radiants.
(define (degrees->radiant degrees)
  (/ degrees 57.2958))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: Structure -> Num
; Interpretation: given a structure, returns the angle of rotation
; of the arrow.
(define (rot arrow)
  (+ 180 (/ (arrow-angle arrow)
            (/ (* (distance (arrow-power arrow)
                            (cond [(< (arrow-angle arrow) 0) 1]
                                  [else
                                   (arrow-angle arrow)])) 10)
                                   1))))

(check-expect (rot initialState) 180)


; Signature: Num Num -> Num
; Interpretation: given a power and an angle, returns the range of the
; arrow (in pixels).
(define (distance power alpha)
  (cond [(= (/ (* 2 (speed-x power alpha) (speed-y power alpha)) G) 0)
         1]
        [else
         (/ (* 2 (speed-x power alpha) (speed-y power alpha)) G)]))

(check-within (distance 80 33) 1006.3149495233043 0.001)
(check-within (distance 100 15) 860.584918868562 0.001)
(check-within (distance 100 45) 1721.1703958689197 0.001)

; Signature: Num Num -> Num
; Interpretation: given a power and an angle, returns the vector
; resulting from the module of x and y vectors.
(define (velocity power alpha)
  (sqrt (+ (* (speed-x power alpha)(speed-x power alpha))
           (* (speed-y power alpha)(speed-y power alpha)))))

(check-within (velocity 100 15) 100.0 0.001)
(check-within (velocity 50 15) 50.0 0.001)


; Signature: Num Num Num Num Num -> Num
; Interpretation: given an initial velocity, an x and y coordinate
; occupied by the arrow, and angle and an x, returns the respective y
; according to the parabola.

(define (trajectory v x0 y0 alpha x)
  (+
   (- 0 (* (/ G (* 2 (* (cond[(< v 1) 1]
                             [else
                              (* v v)]))
                   (* (cos (degrees->radiant
                            (cond [(and (< alpha 1) (>= alpha 0)) 1]
                                  [else alpha])))
                      (cos (degrees->radiant
                            (cond [(and (< alpha 1) (>= alpha 0)) 1]
                                  [else alpha]))))))
           (* x x)))
   (* (+ (/ (* G x0) (* (* (cond[(< v 1) 1]
                                [else (* v v)]))
                        (* (cos (degrees->radiant alpha))
                           (cos (degrees->radiant alpha)))))
         (tan (degrees->radiant alpha))) x)
   (+ (- (- 0 (/ (* G (* x0 x0))
                 (* 2 (* (cond[(< v 1) 1]
                              [else
                               (* v v)]))
                    (* (cos (degrees->radiant alpha))
                       (cos (degrees->radiant alpha))))))
         (tan (degrees->radiant alpha)) x0)
      y0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: Structure -> Num
; Interpretation: given a structure returns the length of the segment formed
; by the two coordinates of the mouse. In other words, it defines the length
; of the segment drawn with the mouse by the user. In the case the segment is
; bigger than 100 px, the value is limited to 100 px.

(define (power arrow) 
  (cond [(<
          (distance-two-points
           (arrow-mx1 arrow) (arrow-my1 arrow)
           (arrow-mx2 arrow) (arrow-my2 arrow)) 200)
         (ceiling (/ (distance-two-points
                      (arrow-mx1 arrow) (arrow-my1 arrow)
                      (arrow-mx2 arrow) (arrow-my2 arrow)) 2))]
        [(>= (distance-two-points
              (arrow-mx1 arrow) (arrow-my1 arrow)
              (arrow-mx2 arrow) (arrow-my2 arrow)) 200)
         100]))

(check-expect (power initialState) 0) 

; Signature: Number -> Number
; Given the range of the arrow, return the x coordinate of the vertex of
; the parabola.
(define (vertex-x distance) 
  (/ distance 2))

; Signature: Number -> Number
; Given the speed and angle of the arrow, return the y coordinate of the vertex
; of the parabola.
(define (vertex-y power alpha)
  (/ (speed-y power alpha) (* 2 G)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Mouse handler
; Signature: Structure Int Int mouse-event -> Structure
; Interpretation: updates the world state after a mouse event.

(define (mouse-handler arrow x y mouse-event)

  ; When the button-down event happens the coordinates of the mouse are saved
  ; in the struct.
  (cond [(and
          (eq? mouse-event "button-down")
          (eq? (arrow-printMessage? arrow) #false))
         (make-arrow
          x y x y
          (arrow-arrowPosX arrow)
          (arrow-arrowPosY arrow)
          (arrow-rot arrow) (arrow-turn arrow) #true
          (arrow-angle arrow) (arrow-power arrow)
          (arrow-life1 arrow) (arrow-life2 arrow)
          (arrow-move arrow)
          (arrow-printMessage? arrow)
          (arrow-shouldStop? arrow)
          (arrow-wind arrow)
          (arrow-out arrow))]

        ; During the drag event the coordinates of the mouse are saved in the struct.
        [(and
          (eq? mouse-event "drag")
          (eq? (arrow-printMessage? arrow) #false))
         (make-arrow
          (arrow-mx1 arrow) (arrow-my1 arrow)
          x y
          (arrow-arrowPosX arrow)
          (arrow-arrowPosY arrow)

          
          (cond
            [(or
              (eq? (arrow-mx1 arrow) x)
              (eq? (arrow-my1 arrow) y))
             (alpha
              (distance-two-points
               (arrow-mx1 arrow) (arrow-my1 arrow)
               (+ x 0.01) (+ y 0.01))
              (abs (- (arrow-mx1 arrow) (+ x 0.01)))
              (- (arrow-my1 arrow) (+ y 0.01)))]
            [else
             (alpha
              (distance-two-points
               (arrow-mx1 arrow) (arrow-my1 arrow)
               x y)
              (abs (- (arrow-mx1 arrow) x))
              (- (arrow-my1 arrow) y))])

          (arrow-turn arrow)
          #true


          ; The angle is computed based on the segment drawn with the mouse.
          (cond
            [(or
              (eq? (arrow-mx1 arrow) x)
              (eq? (arrow-my1 arrow) y))
             (alpha
              (distance-two-points
               (arrow-mx1 arrow) (arrow-my1 arrow)
               (+ x 0.01) (+ y 0.01))
              (abs (- (arrow-mx1 arrow) (+ x 0.01)))
              (- (arrow-my1 arrow) (+ y 0.01)))]
            [else
             (alpha
              (distance-two-points
               (arrow-mx1 arrow) (arrow-my1 arrow)
               x y)
              (abs (- (arrow-mx1 arrow) x))
              (- (arrow-my1 arrow) y))])
          
          (power arrow)
          (arrow-life1 arrow) (arrow-life2 arrow)
          (arrow-move arrow)
          (arrow-printMessage? arrow)
          (arrow-shouldStop? arrow)
          (arrow-wind arrow)
          (arrow-out arrow))]

        ; When the mouse is released, the big bang starts to draw
        ; the moving arrow.

        [(and
          (eq? mouse-event "button-up")
          (eq? (arrow-printMessage? arrow) #false))
         (make-arrow
          (arrow-mx1 arrow) (arrow-my1 arrow)
          (arrow-mx1 arrow) (arrow-my1 arrow)
          (arrow-arrowPosX arrow)
          (arrow-arrowPosY arrow)
          (arrow-rot arrow) (arrow-turn arrow) #false
          (arrow-angle arrow) (arrow-power arrow)
          (arrow-life1 arrow) (arrow-life2 arrow)
          #true
          #false
          #false
          (arrow-wind arrow)
          
          (cond[(and
                 (eq? (arrow-out arrow) #true)
                 (eq? (arrow-move arrow) #false))
                #false]
               [(and
                 (eq? (arrow-out arrow) #false)
                 (eq? (arrow-move arrow) #false))
                #true]
               [else (arrow-out arrow)]))]

        
        ; If the mouse is clicked, dragged or released
        ; while the arrow is moving the game state is not
        ; influenced.
        [(and
          (eq? mouse-event "button-down")
          (eq? (arrow-move arrow) #true)
          (or
           (eq? (arrow-turn arrow) #true)
           (eq? (arrow-turn arrow) #false)))
         arrow]

        [(and
          (eq? mouse-event "drag")
          (eq? (arrow-move arrow) #true)
          (or
           (eq? (arrow-turn arrow) #true)
           (eq? (arrow-turn arrow) #false)))
         arrow]
        
        [(and
          (eq? mouse-event "button-up")
          (eq? (arrow-move arrow) #true)
          (or
           (eq? (arrow-turn arrow) #true)
           (eq? (arrow-turn arrow) #false)))
         arrow]

        ; When the message with the winner is printed,
        ; the mouse actions do not influence the scene.
        [(and
          (or
           (eq? mouse-event "button-up")
           (eq? mouse-event "button-down"))
          (eq? (arrow-printMessage? arrow) #true))
         arrow]

        ; In any other case the current game state is returned.
        [else arrow]))


; Signature: Structure Key -> Structure
; Interpretation: for a given keyboard event return the new state of
; the game.
; When "r" is clicked the initial game state is applied (restart).
; When "q" is clicked the big-bang is closed.
(define (key-handler arrow key)
  (cond
    [(string=? key "r")
     initialState]
    [(string=? key "q")
     (make-arrow
      (arrow-mx1 arrow)
      (arrow-my1 arrow)
      (arrow-mx2 arrow)
      (arrow-my2 arrow)
      130
      350
      0
      (arrow-turn arrow)
      #false
      (arrow-angle arrow)
      (arrow-power arrow)
      (arrow-life1 arrow)
      (arrow-life2 arrow)
      #false
      #true
      #true
      (arrow-wind arrow)
      (arrow-out arrow))]
   
    [else arrow]))

; Signature: Structure -> Boolean
; Interpretation: for a given game state returns true if the big-bang
; should be closed.
(define (shouldStop? arrow)
  (cond [(eq? (arrow-shouldStop? arrow) #true)
         #true]
        [else
         #false]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Defining the function medieval that will be called in the main menu.
; Discussed with TA Long.

(define (medieval a)

  (big-bang initialState
            [to-draw draw-scene]
            [on-tick move-arrow 0.002]
            [display-mode 'normal]
            [on-key key-handler]
            [stop-when shouldStop?]
            [close-on-stop #true]
            [on-mouse mouse-handler]))
