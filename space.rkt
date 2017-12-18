;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname space) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; -- THROWMAN: the game --                                             ;;;;;
;;;;; -- SPACE SCENARIO --                                                 ;;;;;
;;;;; Developed by: Cattaneo Marco, Ferrara Salvatore, Willi Patrizia      ;;;;;
;;;;; Final project - Programming Fundamentals 1, Prof. N. Nystrom         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Importing libraries required

(require 2htdp/image)     ; For images and shapes.
(require 2htdp/universe)  ; For the big bang.
(require racket/base)     ; For providing functions to the main menu.

; Providing the big bang to the main menu.

(provide spacerun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definining the struct that keeps track of the game status.

; An meteora is Struct (make-meteora ...) where:
; - mx1: is the x position of the mouse when the "button down" event happens.
; - my1: is the y position of the mouse when the "button down" event happens.
; - mx2: is the x position of the mouse when the "drag" event happens.
; - my2: is the y position of the mouse when the "drag" event happens.
; - meteoraPosX: is the x coordinate where the meteora should be drawn.
; - meteoraPosX: is the y coordinate where the meteora should be drawn.
; - rot: is the rotation of the meteora on its pinhole (Int).
; - turn: is a Boolean, when true is player one's turn.
; - power: is an Int [0,100] and defines the speed of the object being shot.
; - life1: is an Int [0,100] that keeps track of the life of the first player.
; - life2: is an Int [0,100] that keeps track of the life of the second player.
; - move: is a Boolean that describes when the meteora should move according to
;         its trajectory.
; - printMessage?: is a Boolean that when true triggers function that returns
;                  the message with the winner of the match.
; - shouldStop?: is a Boolean that defines when the big bang should stop.
; - gravity is an random value between [-10, 10] that defines the intensity of the
;         gravity. This value starts at zero and changes every turn.
; - out: is a Boolean that becomes true when the projectile is out of scene
;        (left, right or bottom).

(define-struct meteora
  [mx1 my1 mx2 my2
       meteoraPosX meteoraPosY
       rot
       turn line
       angle power
       life1 life2
       move printMessage?
       shouldStop?
       gravity
       out])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Building the scenario of the game.

; Defining the WIDTH of the scene in pixels.
(define WIDTH 1400)

; The constant BACKGROUND is constant that represent the image of the scenario.
(define BACKGROUND (bitmap "moonback.jpg"))

; Defining the two players (astronauts).

(define playerOne (bitmap "ASTRONAUT1.png"))
(define playerTwo (bitmap "ASTRONAUT2.png"))


; players is list with the players, playerOne and playerTwo.

(define players
  (list playerOne
        playerTwo))

; positions is a list of the positions occupied by the two players on the scene.

(define positions
  (list
   (make-posn 64 300)
   (make-posn (- WIDTH 64) 300)))

; The variable SCENARIO is an image with the players and the background.

(define SCENARIO
  (place-images
   players
   positions
   BACKGROUND))


; initialState is a (make-meteora) with the initial game scenario. 
(define initialState
  (make-meteora 0 0 0 0 130 190 0 #true #false 0 0 100 100 #false #false #false 6 #true))

; METEORA is a constant that defines the picture of the meteora.
(define METEORA
  (scale 0.25 (bitmap "asteroid.png")))

; TURN is a yellow triangle used to indicate the turn.
(define TURN
  (rotate 180(triangle 25 "solid" "gold")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following part of the code defines the conditions that indicates when a
; player has been hit by the object.

; Signature: Int -> Boolean
; Interpretation: for a given x returns if this x is occupied by a player.

(define (hasHitP2?X x)
  (cond[(and
         (< x (- WIDTH 50))
         (> x (- WIDTH 120)))
        #true]
       [else #false]))

(check-expect (hasHitP2?X 1300) #true)
(check-expect (hasHitP2?X 200) #false)
(check-expect (hasHitP2?X 50) #false)

; Signature: Int -> Boolean
; Interpretation: for a given y returns if this y is occupied by a player.
(define (hasHitP2?Y y)
  (cond[(and
         (> y 50)
         (< y 155))
        #true]
       [else #false]))

(check-expect (hasHitP2?Y 100) #true)
(check-expect (hasHitP2?Y -20) #false)
(check-expect (hasHitP2?Y 50) #false)

; Signature: Int -> Boolean
; Interpretation: checks whether the meteora is permanently out of the scene on
; the x axis.
(define (isOut? x)
  (cond[(or
         (<= x 0)
         (>= x WIDTH))
        #true]
       [else #false]))

(check-expect (isOut? -10) #true)
(check-expect (isOut? 500) #false)
(check-expect (isOut? 1200) #false)

; Signature: Int -> Boolean
; Interpretation: checks whether the meteora has hit ground.
(define (hasHitGround? x)
  (cond[(or
         (>= x 370)
         (<= x 0))
        #true]
       [else #false]))

(check-expect (hasHitGround? 0) #true)
(check-expect (hasHitGround? 300) #false)
(check-expect (hasHitGround? -10) #true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Defining life and gravity bars.


; Signature: Int -> Image
; Interpretation: for a given life (Int [0,100]) returns a in image of
; the life bar.
(define (HP-line x)
  (overlay/align "center" "center"
                 (text
                  (string-append (number->string x) "%") 12 "white")
                 (overlay/align "left" "middle"
                                (cond [(> x 0)
                                       (rectangle (* 2 x)
                                                  20 "solid" "forestgreen")]
                                      [else
                                       (rectangle 0
                                                  20 "solid" "forestgreen")])
                                (rectangle 200 20 "solid" "crimson"))))

                                      
; Signature: Int -> Image
; Interpretation: for a given gravity (Int [1,10]) returns an image of the
; gravity bar.                        

(define (GRAVITY-line meteora)
  (overlay/xy
   (rectangle (* 1.4 10 (meteora-gravity meteora)) 8 "solid" "black")
   0 0
   (rectangle 140 8 "outline" "YellowGreen")))
         

  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Function that draws the scene.

; Signature: Struct -> Image
; Interpretation: for a given struct returns the new world state.
(define (draw-scene meteora)
  (cond

    ; Condition that draws the line and overlays the various strings
    ; indicating the angle and speed of the meteora.
    [(and
      (eq? (meteora-move meteora) #false)
      (eq? (meteora-line meteora) #true))
     (overlay/xy
      TURN
      (cond [(eq? (meteora-out meteora) #true)
             -85]

            [else
             (- (- 0 WIDTH) -112)]) 
      -210
      (overlay/xy
       (GRAVITY-line meteora)
       -630 -73
       (overlay/xy
        (rotate 15 (HP-line (meteora-life1 meteora)))
        -20
        -30                  
        (overlay/xy 
         (rotate -15 (HP-line (meteora-life2 meteora)))
         (+ (- 0 WIDTH) 230)
         -20
         (overlay/align "center" "bottom"
                        (text
                         (string-append "Angle: "
                                        (number->string
                                         (floor (meteora-angle meteora))) "°"
                                        " - Speed: "
                                        (number->string (meteora-power meteora)))
                         13 "white")
                        
                        (add-line
                         SCENARIO 
                         (meteora-mx1 meteora)
                         (meteora-my1 meteora)
                         (meteora-mx2 meteora)
                         (meteora-my2 meteora)
                         "white"))))))]
    
    ; Condition that draws the moving meteora when the
    ; it's first player's turn.
    [(and
      (eq? (meteora-line meteora) #false)
      (eq? (meteora-move meteora) #true)
      (eq? (meteora-turn meteora) #true))
     (place-image
      (rotate (meteora-rot meteora) METEORA)
      (meteora-meteoraPosX meteora)
      (- 400 (meteora-meteoraPosY meteora))

      (overlay/xy
       TURN
       (cond [(eq? (meteora-out meteora) #true)
              -85]

             [else
              (- (- 0 WIDTH) -112)]) 
       -210
       (overlay/xy
        (rotate 15 (HP-line (meteora-life1 meteora)))
        -20
        -30                              
        (overlay/xy
         (GRAVITY-line meteora)
         -630 -73
         (overlay/xy 
          (rotate -15 (HP-line (meteora-life2 meteora)))
          (+ (- 0 WIDTH) 230)
          -20
          SCENARIO)))))] 

    ; Condition that draws the moving meteora when the
    ; it's second player's turn.
    [(and
      (eq? (meteora-line meteora) #false)
      (eq? (meteora-move meteora) #true)
      (eq? (meteora-turn meteora) #false))
         
     (place-image
      (rotate  (- 180 (meteora-rot meteora)) METEORA)
      (- WIDTH (meteora-meteoraPosX meteora))
      (- 400 (meteora-meteoraPosY meteora))         
      (overlay/xy
       TURN
       (cond [(eq? (meteora-out meteora) #true)
              -85]
             [else
              (- (- 0 WIDTH) -112)]) 
       -210
       (overlay/xy
        (GRAVITY-line meteora)
        -630 -73
        (overlay/xy
         (rotate 15 (HP-line (meteora-life1 meteora)))
         -20
         -30   
         (overlay/xy 
          (rotate -15 (HP-line (meteora-life2 meteora)))
          (+ (- 0 WIDTH) 230)
          -20
          SCENARIO)))))]
    
    ; Condition that prints the message "Player 2 WINS!" when
    ; the life of the first player is lower or equal to zero.
    [(and
      (<= (meteora-life1 meteora) 0)
      (eq? (meteora-printMessage? meteora) #true))
         
         
     (overlay/align "center" "center"
                    (text
                     "Player 2 WINS!"
                     40 "white")
                    SCENARIO)]

    ; Condition that prints the message "Player 1 WINS!" when
    ; the life of the second player is lower or equal to zero.    
    [(and
      (<= (meteora-life2 meteora) 0)
      (eq? (meteora-printMessage? meteora) #true))
         
     (overlay/align "center" "center"
                    (text
                     "Player 1 WINS!"
                     40 "white")
                    SCENARIO)]
         

     ; This condition draws the scenario when no line is being drawn and
    ; the meteora is not moving.
    ; In this case the TURN indicator, the gravity and life bars are drawn
    ; over the image.
    [(and
      (eq? (meteora-move meteora) #false)
      (eq? (meteora-line meteora) #false))
     (overlay/xy
      TURN
      (cond [(eq? (meteora-out meteora) #true)
             -85]
            [else
             (- (- 0 WIDTH) -112)]) 
      -210
      (overlay/xy
       (GRAVITY-line meteora)
       -630 -73
       (overlay/xy
        (rotate 15 (HP-line (meteora-life1 meteora)))
        -20
        -30
        (overlay/xy 
         (rotate -15 (HP-line (meteora-life2 meteora)))
         (+ (- 0 WIDTH) 230)
         -20
         SCENARIO))))]

    ; In the case none of the previous conditions is met,
    ; the TURN indicator, the gravity and life bars are drawn over the image.
    [else
     (overlay/xy
      TURN
      (cond [(eq? (meteora-out meteora) #true)
             -85]
            [else
             (- (- 0 WIDTH) -112)]) 
      -210
      (overlay/xy
       (GRAVITY-line meteora)
       -630 -73
       (overlay/xy
        (rotate 15 (HP-line (meteora-life1 meteora)))
        -20
        -30
        (overlay/xy 
         (rotate -15 (HP-line (meteora-life2 meteora)))
         (+ (- 0 WIDTH) 230)
         -20
         SCENARIO))))]))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; On tick function

; Signature: Structure -> Structure
; Interpretation: updates a given structure every tick according to the various
; changes in the game state.


(define (move-meteora meteora)
  ; When there is no message printed and no line being drawn and the
  ; meteora is moving on the scene and it's player one's turn.
  (cond[(and
         (eq? (meteora-printMessage? meteora) #false)
         (eq? (meteora-line meteora) #false)
         (eq? (meteora-move meteora) #true)
         (eq? (meteora-turn meteora) #true))
        
        (make-meteora
         (meteora-mx1 meteora)
         (meteora-my1 meteora)
         (meteora-mx2 meteora)
         (meteora-my2 meteora)

         ; The x position of the meteora is updated only if the rival player
         ; is not hit. When the rival player is hit the meteora stops.
         ; The x position is updated based on the power defined by the user
         ; with the mouse interaction.
         (cond[(and
                (hasHitP2?X (meteora-meteoraPosX meteora))
                (hasHitP2?Y (meteora-meteoraPosY meteora)))
               (meteora-meteoraPosX meteora)]
              [else
               (+ (speed-x (/ (meteora-power meteora) 3) (meteora-angle meteora))
                  (meteora-meteoraPosX meteora))])

         ; The y position of the meteora is updated only if the rival player
         ; is not hit. When the rival player is hit the meteora stops.
         ; The y position is updated according to the external function
         ; trajectory that computes the exact parabola followed by the meteora
         ; during its fly. The trajectory is also influenced by the gravity.
         (cond [(and
                 (hasHitP2?Y (meteora-meteoraPosY meteora))
                 (hasHitP2?X (meteora-meteoraPosX meteora)))
                (meteora-meteoraPosY meteora)]
               [else
                (trajectory
                 (velocity (meteora-power meteora)(meteora-angle meteora))
                 100 180
                 (meteora-angle meteora)
                 (meteora-meteoraPosX meteora)(meteora-gravity meteora))])
                
         ; The rotation of the meteora is updated while moving.
         (- (meteora-rot meteora) (* (rot meteora) 400))


         
         (meteora-turn meteora) 
         (meteora-line meteora)
         (meteora-angle meteora)
         (meteora-power meteora)
         (meteora-life1 meteora)

         ; Defining the damage inflicted by the meteora on the player.
         
         ; When the head is hit 50 of 100 life points are deducted by the
         ; current life of the player.
         (cond[(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora))
                (and
                 (<= (meteora-meteoraPosY meteora) 155)
                 (> (meteora-meteoraPosY meteora) 106)))
               (- (meteora-life2 meteora) 50)]

              ; When the chest is hit 30 of 100 life points are deducted by the
              ; current life of the player.
              [(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora)) 
                (and
                 (<= (meteora-meteoraPosY meteora) 106)
                 (> (meteora-meteoraPosY meteora) 78)))
               (- (meteora-life2 meteora) 30)]

              ; When the lower body is hit, 20 of 100 life points are deducted
              ; by the current life of the player.
              [(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora))
                (and
                 (<= (meteora-meteoraPosY meteora) 78)
                 (>= (meteora-meteoraPosY meteora) 50)))
               (- (meteora-life2 meteora) 20)]

              ; In other cases the life is not changed.
              [else
               (meteora-life2 meteora)])

         ; When the player is hit by the meteora the meteora stops.
         ; If no player is hit the meteora keeps moving until it is out of
         ; the scene.
         (cond[(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora)))
               #false]
              [else
               #true])
         
         #false
         #false
         (meteora-gravity meteora)
         (meteora-out meteora))]

       ;;;;;;;;

       ; When there is no message printed and no line being drawn and the
       ; meteora is moving on the scene and it's player two's turn.
       [(and
         (eq? (meteora-line meteora) #false)
         (eq? (meteora-printMessage? meteora) #false)
         (eq? (meteora-move meteora) #true)
         (eq? (meteora-turn meteora) #false))
        
        (make-meteora
         (meteora-mx1 meteora)
         (meteora-my1 meteora)
         (meteora-mx2 meteora)
         (meteora-my2 meteora)

         ; The x position of the meteora is updated only if the rival player
         ; is not hit. When the rival player is hit the meteora stops.
         ; The x position is updated based on the power defined by the user
         ; with the mouse interaction.
         (cond [(and
                 (hasHitP2?X (meteora-meteoraPosX meteora))
                 (hasHitP2?Y (meteora-meteoraPosY meteora)))
                (meteora-meteoraPosX meteora)]
               [else
                (+ (speed-x (/ (meteora-power meteora) 3)
                            (meteora-angle meteora))
                   (meteora-meteoraPosX meteora))])

         
         ; The y position of the meteora is updated only if the rival player
         ; is not hit. When the rival player is hit the meteora stops.
         ; The y position is updated according to the external function
         ; trajectory that computes the exact parabola followed by the meteora
         ; during its fly. The trajectory is also influenced by the gravity.
         (cond [(and
                 (hasHitP2?Y (meteora-meteoraPosY meteora))
                 (hasHitP2?X (meteora-meteoraPosX meteora)))
                (meteora-meteoraPosY meteora)]
               [else                                                  
                (trajectory
                 (velocity (meteora-power meteora)(meteora-angle meteora))
                 100 180
                 (meteora-angle meteora)
                 (meteora-meteoraPosX meteora)(meteora-gravity meteora))])
                                                           
         ; The rotation of the meteora is updated while moving.                                         
         (-  (meteora-rot meteora)  (* (rot meteora) 400)) 

         (meteora-turn meteora)
         (meteora-line meteora)
         (meteora-angle meteora)
         (meteora-power meteora)

         ; Defining the damage inflicted by the meteora on the player.
         
         ; When the head is hit 50 of 100 life points are deducted by the
         ; current life of the player.
         (cond[(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora))
                (and
                 (<= (meteora-meteoraPosY meteora) 155)
                 (> (meteora-meteoraPosY meteora) 106)))
               (- (meteora-life1 meteora) 50)]

              ; When the chest is hit 30 of 100 life points are deducted by the
              ; current life of the player.
              [(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora)) 
                (and
                 (<= (meteora-meteoraPosY meteora) 106)
                 (> (meteora-meteoraPosY meteora) 78)))
               (- (meteora-life1 meteora) 30)]

              ; When the lower body is hit, 20 of 100 life points are deducted
              ; by the current life of the player.
              [(and
                (hasHitP2?Y (meteora-meteoraPosY meteora))
                (hasHitP2?X (meteora-meteoraPosX meteora))
                (and
                 (<= (meteora-meteoraPosY meteora) 78)
                 (>= (meteora-meteoraPosY meteora) 50)))
               (- (meteora-life1 meteora) 20)]

              ; In other cases the life is not changed.
              [else
               (meteora-life1 meteora)])
          
         (meteora-life2 meteora)
         
         ; When the player is hit by the meteora the meteora stops.
         ; If no player is hit the meteora keeps moving until it is out of
         ; the scene.
         (cond [(and
                 (hasHitP2?Y (meteora-meteoraPosY meteora))
                 (hasHitP2?X (meteora-meteoraPosX meteora)))
                #false]
               [else
                #true])
         
         #false
         #false
         (meteora-gravity meteora)
         (meteora-out meteora))]


       ;;;;;;;
       
       ; When it's first player's turn and the meteora
       ; goes out of the scene without hitting any player.

       [(and
         (eq? (meteora-turn meteora) #true)
         (eq? (meteora-printMessage? meteora) #false)
         (or
          (isOut? (meteora-meteoraPosX meteora))
          (hasHitGround? (meteora-meteoraPosY meteora))
          (< (meteora-meteoraPosY meteora) 0)
          (and
           (hasHitP2?X (meteora-meteoraPosX meteora))
           (hasHitP2?Y (meteora-meteoraPosY meteora)))))

        ; The structure is redefined, turn changed and
        ; the starting position of the meteora is also restored.
        ; A new random value of gravity is created.        
        (make-meteora
         (meteora-mx1 meteora)
         (meteora-my1 meteora)
         (meteora-mx2 meteora)
         (meteora-my2 meteora)
         130
         190
         180
         #false
         (meteora-line meteora)
         (meteora-angle meteora)
         (meteora-power meteora)
         (meteora-life1 meteora)
         (meteora-life2 meteora)
         #false
         #false
         #false
         (random 1 10)
         (meteora-out meteora))]


       ;;;;;;

       ; When it's first player's turn and the meteora
       ; goes out of the scene without hitting any player.
       [(and
         (eq? (meteora-turn meteora) #false)
         (eq? (meteora-printMessage? meteora) #false)
         (or
          (isOut? (meteora-meteoraPosX meteora))
          (hasHitGround? (meteora-meteoraPosY meteora))
          (< (meteora-meteoraPosY meteora) 0)
          (and
           (hasHitP2?X (meteora-meteoraPosX meteora))
           (hasHitP2?Y (meteora-meteoraPosY meteora)))))

        ; The structure is redefined, turn changed and
        ; the starting position of the meteora is also restored.
        ; A new random value of gravity is created. 
        (make-meteora
         (meteora-mx1 meteora)
         (meteora-my1 meteora)
         (meteora-mx2 meteora)
         (meteora-my2 meteora)
         130
         190
         0
         #true
         (meteora-line meteora)
         (meteora-angle meteora)
         (meteora-power meteora)
         (meteora-life1 meteora)
         (meteora-life2 meteora)
         #false
         #false
         #false
         (random 1 10)
         (meteora-out meteora))]
        

       ; When one of the two players' life is equal or lower than zero a
       ; message with the winner is displayed.
       [(and
         (eq? (meteora-printMessage? meteora) #false)
         (or
          (eq? (meteora-turn meteora) #false)
          (eq? (meteora-turn meteora) #true))
         (or
          (<= (meteora-life1 meteora) 0)
          (<= (meteora-life2 meteora) 0)))
        (make-meteora
         (meteora-mx1 meteora)
         (meteora-my1 meteora)
         (meteora-mx2 meteora)
         (meteora-my2 meteora)
         130
         190
         0
         #true
         #false
         (meteora-angle meteora)
         (meteora-power meteora)
         (meteora-life1 meteora)
         (meteora-life2 meteora)
         #false
         #true
         #false
         (meteora-gravity meteora)
         (meteora-out meteora))]
       
       ; In any other case the unmodified struct is given to the
       ; big bang.
       [else meteora]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; Defining the initial velocity as a product of the power chosen by
; the user.
; Signature: Structure -> Num
; Interpretation: given a structure, returns the initial velocity.
(define (initial-velocity meteora)
  (* (/ (meteora-power meteora) 100) (* WIDTH 10)))

; Signature: Num -> Num
; Interpretation: given an angle in degrees, returns the angle in
; radiants.
(define (degrees->radiant degrees)
  (/ degrees 57.2958))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: Structure -> Num
; Interpretation: given a structure, returns the angle of rotation
; of the meteora.
(define (rot meteora)
  (+ 180 (/ (meteora-angle meteora)
            (/ (* (distance (meteora-power meteora)
                            (cond [(< (meteora-angle meteora) 0) 1]
                                  [else
                                   (meteora-angle meteora)])
                            (meteora-gravity meteora)) 10)
               1))))
  
(check-expect (rot initialState) 180)

; Signature: Num Num -> Num
; Interpretation: given a power and an angle, returns the range of the
; meteora (in pixels).
(define (distance power alpha gravity)
  (cond [(= (/ (* 2 (speed-x power alpha) (speed-y power alpha)) gravity) 0)
         1]
        [else
         (/ (* 2 (speed-x power alpha) (speed-y power alpha)) gravity)]))


; Signature: Num Num -> Num
; Interpretation: given a power and an angle, returns the vector
; resulting from the module of x and y vectors.

(define (velocity power alpha)
  (sqrt (+ (* (speed-x power alpha)(speed-x power alpha))
           (* (speed-y power alpha)(speed-y power alpha)))))



; Signature: Num Num Num Num Num -> Num
; Interpretation: given an initial velocity, an x and y coordinate
; occupied by the meteora, and angle and an x, returns the respective y
; according to the parabola.

(define (trajectory v x0 y0 alpha x gravity)
  (+
   (- 0 (* (/ gravity (* 2 (* (cond[(< v 1) 1]
                                   [else
                                    (* v v)]))
                         (* (cos (degrees->radiant
                                  (cond [(and (< alpha 1) (>= alpha 0)) 1]
                                        [else alpha])))
                            (cos (degrees->radiant
                                  (cond [(and (< alpha 1) (>= alpha 0)) 1]
                                        [else alpha])))))) (* x x)))
   (* (+ (/ (* gravity x0) (* (* (cond[(< v 1) 1]
                                      [else
                                       (* v v)]))
                              (* (cos (degrees->radiant alpha))
                                 (cos (degrees->radiant alpha)))))
         (tan (degrees->radiant alpha))) x)
   (+ (- (- 0 (/ (* gravity (* x0 x0)) (* 2 (* (cond[(< v 1) 1]
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

(define (power meteora) 
  (cond [(<
          (distance-two-points
           (meteora-mx1 meteora) (meteora-my1 meteora)
           (meteora-mx2 meteora) (meteora-my2 meteora)) 200)
         (ceiling (/ (distance-two-points
                      (meteora-mx1 meteora) (meteora-my1 meteora)
                      (meteora-mx2 meteora) (meteora-my2 meteora)) 2))]
        [(>= (distance-two-points
              (meteora-mx1 meteora) (meteora-my1 meteora)
              (meteora-mx2 meteora) (meteora-my2 meteora)) 200)
         100]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Mouse handler
; Signature: Structure Int Int mouse-event -> Structure
; Interpretation: updates the world state after a mouse event.

(define (mouse-handler meteora x y mouse-event)

  ; When the button-down event happens the coordinates of the mouse
  ; are saved in the struct.
  (cond [(and
          (eq? mouse-event "button-down")
          (eq? (meteora-printMessage? meteora) #false))
         (make-meteora
          x y x y
          (meteora-meteoraPosX meteora)
          (meteora-meteoraPosY meteora)
          (meteora-rot meteora) (meteora-turn meteora) #true
          (meteora-angle meteora) (meteora-power meteora)
          (meteora-life1 meteora) (meteora-life2 meteora)
          (meteora-move meteora)
          (meteora-printMessage? meteora)
          (meteora-shouldStop? meteora)
          (meteora-gravity meteora)
          (meteora-out meteora))]

        ; During the drag event the coordinates of the mouse are saved in
        ; the struct.
        [(and
          (eq? mouse-event "drag")
          (eq? (meteora-printMessage? meteora) #false))
         (make-meteora
          (meteora-mx1 meteora) (meteora-my1 meteora)
          x y
          (meteora-meteoraPosX meteora)
          (meteora-meteoraPosY meteora)
          
          (cond
            [(or
              (eq? (meteora-mx1 meteora) x)
              (eq? (meteora-my1 meteora) y))
             (alpha
              (distance-two-points
               (meteora-mx1 meteora) (meteora-my1 meteora)
               (+ x 0.01) (+ y 0.01))
              (abs (- (meteora-mx1 meteora) (+ x 0.01)))
              (- (meteora-my1 meteora) (+ y 0.01)))]
            [else
             (alpha
              (distance-two-points
               (meteora-mx1 meteora) (meteora-my1 meteora)
               x y)
              (abs (- (meteora-mx1 meteora) x))
              (- (meteora-my1 meteora) y))])

          (meteora-turn meteora)
          #true

          ; The angle is computed based on the segment drawn with the mouse.
          (cond
            [(or
              (eq? (meteora-mx1 meteora) x)
              (eq? (meteora-my1 meteora) y))
             (alpha
              (distance-two-points
               (meteora-mx1 meteora) (meteora-my1 meteora)
               (+ x 0.01) (+ y 0.01))
              (abs (- (meteora-mx1 meteora) (+ x 0.01)))
              (- (meteora-my1 meteora) (+ y 0.01)))]
            [else
             (alpha
              (distance-two-points
               (meteora-mx1 meteora) (meteora-my1 meteora)
               x y)
              (abs (- (meteora-mx1 meteora) x))
              (- (meteora-my1 meteora) y))])
          
          (power meteora)
          (meteora-life1 meteora) (meteora-life2 meteora)
          (meteora-move meteora)
          (meteora-printMessage? meteora)
          (meteora-shouldStop? meteora)
          (meteora-gravity meteora)
          (meteora-out meteora))]

        ; When the mouse is released, the big bang starts to draw
        ; the moving meteora.
        [(and
          (eq? mouse-event "button-up")
          (eq? (meteora-printMessage? meteora) #false))
         (make-meteora
          (meteora-mx1 meteora) (meteora-my1 meteora)
          (meteora-mx1 meteora) (meteora-my1 meteora)
          (meteora-meteoraPosX meteora)
          (meteora-meteoraPosY meteora)
          (meteora-rot meteora) (meteora-turn meteora) #false
          (meteora-angle meteora) (meteora-power meteora)
          (meteora-life1 meteora) (meteora-life2 meteora)
          #true
          #false
          #false
          (meteora-gravity meteora)
          (cond[(and
                 (eq? (meteora-out meteora) #true)
                 (eq? (meteora-move meteora) #false))
                #false]
               [(and
                 (eq? (meteora-out meteora) #false)
                 (eq? (meteora-move meteora) #false))
                #true]
               [else (meteora-out meteora)]))]

        ; If the mouse is clicked, dragged or released
        ; while the meteora is moving the game state is not
        ; influenced.        
        [(and
          (eq? mouse-event "button-down")
          (eq? (meteora-move meteora) #true)
          (or
           (eq? (meteora-turn meteora) #true)
           (eq? (meteora-turn meteora) #false)))
         meteora]

        [(and
          (eq? mouse-event "drag")
          (eq? (meteora-move meteora) #true)
          (or
           (eq? (meteora-turn meteora) #true)
           (eq? (meteora-turn meteora) #false)))
         meteora]
        
        [(and
          (eq? mouse-event "button-up")
          (eq? (meteora-move meteora) #true)
          (or
           (eq? (meteora-turn meteora) #true)
           (eq? (meteora-turn meteora) #false)))
         meteora]

        ; When the message with the winner is printed,
        ; the mouse actions do not influence the scene.
        [(and
          (or
           (eq? mouse-event "button-up")
           (eq? mouse-event "button-down"))
          (eq? (meteora-printMessage? meteora) #true))
         meteora]

        ; In any other case the current game state is returned.
        [else meteora]))


  
; Signature: Structure Key -> Structure
; Interpretation: for a given keyboard event return the new state of
; the game.
; When "r" is clicked the initial game state is applied (restart).
; When "q" is clicked the big-bang is closed.
(define (key-handler meteora key)
  (cond
    [(string=? key "r")
     initialState]
    [(string=? key "q")
     (make-meteora
      (meteora-mx1 meteora)
      (meteora-my1 meteora)
      (meteora-mx2 meteora)
      (meteora-my2 meteora)
      130
      350
      0
      (meteora-turn meteora)
      #false
      (meteora-angle meteora)
      (meteora-power meteora)
      (meteora-life1 meteora)
      (meteora-life2 meteora)
      #false
      #true
      #true
      (meteora-gravity meteora)
      (meteora-out meteora))]
   
    [else meteora]))

; Signature: Structure -> Boolean
; Interpretation: for a given game state returns true if the big-bang
; should be closed.
(define (shouldStop? meteora)
  (cond [(eq? (meteora-shouldStop? meteora) #true)
         #true]
        [else
         #false]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Defining the function space that will be called in the main menu.
; Discussed with TA Long.

(define (spacerun a)
  
  (big-bang initialState
            [to-draw draw-scene]
            [on-tick move-meteora 0.025]
            [display-mode 'normal]
            [on-key key-handler]
            [stop-when shouldStop?]
            [close-on-stop #true]
            [on-mouse mouse-handler]))
