;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname christmas) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; -- THROWMAN: the game --                                             ;;;;;
;;;;; -- CHRISTMAS SCENARIO --                                             ;;;;;
;;;;; Developed by: Cattaneo Marco, Ferrara Salvatore, Willi Patrizia      ;;;;;
;;;;; Final project - Programming Fundamentals 1, Prof. N. Nystrom         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Importing packages required

(require 2htdp/image)      ; For images and shapes.
(require 2htdp/universe)   ; For the Big Bang.
(require racket/base)      ; For providing functions to the main menu.

(provide christmasrun)     ; Providing the Big Bang to the main menu.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definining the struct that keeps track of the game status.

; An gift is Struct (make-gift ...) where:
; - mx1: is the x position of the mouse when the "button down" event happens.
; - my1: is the y position of the mouse when the "button down" event happens.
; - mx2: is the x position of the mouse when the "drag" event happens.
; - my2: is the y position of the mouse when the "drag" event happens.
; - giftPosX: is the x coordinate where the gift should be drawn.
; - giftPosX: is the y coordinate where the gift should be drawn.
; - rot: is the rotation of the gift on its pinhole (Int).
; - turn: is a Boolean, when true is player one's turn.
; - power: is an Int [0,100] and defines the speed of the object being shot.
; - life1: is an Int [0,100] that keeps track of the life of the first player.
; - life2: is an Int [0,100] that keeps track of the life of the second player.
; - move: is a Boolean that describes when the gift should move according to
;         its trajectory.
; - printMessage?: is a Boolean that when true triggers function that returns
;                  the message with the winner of the match.
; - shouldStop?: is a Boolean that defines when the big bang should stop.
; - wind: is an random value between [-10, 10] that defines the intensity of the
;         wind. This value starts at zero and changes every turn.
; - out: is a Boolean that becomes true when the projectile is out of scene
;        (left, right or bottom).

(define-struct gift
  [mx1 my1 mx2 my2
       giftPosX giftPosY
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

; Defining the WIDTH of the scene
(define WIDTH 1200)


; The variable SCENARIO is constant that represent the image of the scenario.
(define SCENARIO
  (bitmap "xmas.jpg"))

; initialState is a (make-gift) with the initial game scenario. 

(define initialState
  (make-gift 0 0 0 0
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

; gift is a constant that defines the picture of the gift.
(define GIFT
  (scale
   0.12
   (bitmap "pack.png")))

; TURN is a yellow triangle used to indicate the turn.
(define TURN
  (rotate 180 (triangle 25 "solid" "yellow")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following part of the code defines the conditions that indicates when a
; player has been hit by the object.


; Signature: Int -> Boolean
; Interpretation: for a given x returns if this x is occupied by the player one.
(define (hasHitP2?X x)
  (cond[(and
         (< x (- WIDTH 50))
         (> x (- WIDTH 150)))
        #true]
       [else #false]))

(check-expect (hasHitP2?X 1100) #true)
(check-expect (hasHitP2?X 200) #false)
(check-expect (hasHitP2?X 50) #false)

; Signature: Int -> Boolean
; Interpretation: for a given y returns if this y is occupied by the player two.
(define (hasHitP2?Y y)
  (cond[(and
         (> y 50)
         (< y 200))
        #true]
       [else #false]))


(check-expect (hasHitP2?Y 100) #true)
(check-expect (hasHitP2?Y -20) #false)
(check-expect (hasHitP2?Y 60) #true)


; Signature: Num Num -> Boolean
; Interpretation: checks whether the gift hit the wall

(define (wall? x y)
  (cond 
    [(and
      (and
       (< x (- WIDTH 400))
       (> x 400))
      (and
       (< y 120)
       (> y 20 )))          
     #true]

    [(and
      (and
       (< x (- WIDTH 470))
       (> x 470))
      (and
       (< y 240)
       (> y 121 ))) 
     #true]
    
    [(and
      (and
       (< x (- WIDTH 520))
       (> x 520))
      (and
       (< y 380)
       (> y 241 )))   
     #true]
    
    [else #false]))

; Signature: Int -> Boolean
; Interpretation: checks whether the gift is permanently out of the scene on
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
; Interpretation: checks whether the gift has hit ground.
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

; Signature: Int -> Image
; Interpretation: for a given life (Int [0,100]) returns a in image of
; the life bar.
(define (HP-line x)
  (overlay/align "center" "center"
                 (overlay/align "center" "center"
                                (text (string-append
                                       (number->string x) "%")
                                      12 "black")
                                (overlay/align "left" "middle"
                                               (cond [(> x 0)
                                                      (rectangle (* 2 x) 18 "solid" "white")]
                                                     [else
                                                      (rectangle 0 20 "solid" "white")])
                                               (rectangle 200 18 "solid" "crimson")))
                 (rectangle 202 20 "outline" "white")))


; Signature: Int -> Image
; Interpretation: for a given wind (Int [-10,10]) returns an image of the
; wind bar.
(define (WIND-line gift)
  (cond [(<= (gift-wind gift) 0)
         (overlay/offset
          (rectangle (* 100 (abs (gift-wind gift))) 20 "solid" "black")
          (- 0 (/ (* 100 (gift-wind gift)) 2)) 0
          (rectangle 200 20 "outline" "white"))]

        [(> (gift-wind gift) 0)
         (overlay/offset
          (rectangle (* 100 (gift-wind gift)) 20 "solid" "black")
          (- 0 (/ (* 100 (gift-wind gift)) 2)) 0
          (rectangle 200 20 "outline" "white"))]))
         

  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Function that draws the scene.
; Signature: Struct -> Image
; Interpretation: for a given struct returns the new world state.

(define (draw-scene gift)
  ; Condition that draws the line and overlays the various strings
  ; indicating the angle and speed of the gift.
  
  (cond [(and
          (eq? (gift-move gift) #false)
          (eq? (gift-line gift) #true))
         (overlay/xy
          TURN
          (cond [(eq? (gift-out gift) #true)
                 -95]

                [else
                 (- (- 0 WIDTH) -175)]) 
          -400
          (overlay/xy
           (WIND-line gift)
           -505 -33
           (overlay/xy
            (HP-line (gift-life1 gift))
            -97 -60
            (overlay/xy
             (HP-line (gift-life2 gift))
             (- (- WIDTH 280)) -60
             (overlay/align "center" "bottom"
                            (text
                             (string-append "Angle: "
                                            (number->string
                                             (floor (gift-angle gift))) "°"
                                                                          " - Speed: "
                                                                          (number->string (gift-power gift)))
                             13 "grey")
                            
                            (add-line 
                             SCENARIO
                             (gift-mx1 gift)
                             (gift-my1 gift)
                             (gift-mx2 gift)
                             (gift-my2 gift)
                             "black"))))))]

        ; Condition that draws the moving gift when the
        ; it's first player's turn.
        [(and
          (eq? (gift-line gift) #false)
          (eq? (gift-move gift) #true)
          (eq? (gift-turn gift) #true))
         
         (place-image
          (rotate (gift-rot gift) GIFT)
          (gift-giftPosX gift)
          (- 650 (gift-giftPosY gift))
          (overlay/xy
           TURN
           (cond [(eq? (gift-out gift) #true)
                  -95]
                 [else
                  (- (- 0 WIDTH) -175)]) 
           -400
           (overlay/xy
            (HP-line (gift-life1 gift))
            -97 -60                            
            (overlay/xy
             (WIND-line gift)
             -505 -33
             (overlay/xy
              (HP-line (gift-life2 gift))
              (- (- WIDTH 280)) -60
              SCENARIO)))))]

        ; Condition that draws the moving gift when the
        ; it's second player's turn.
        [(and
          (eq? (gift-line gift) #false)
          (eq? (gift-move gift) #true)
          (eq? (gift-turn gift) #false))
         
         (place-image
          (rotate  (- 180 (gift-rot gift)) GIFT)
          (- WIDTH (gift-giftPosX gift))
          (- 650 (gift-giftPosY gift))

          (overlay/xy
           TURN
           (cond [(eq? (gift-out gift) #true)
                  -95]

                 [else
                  (- (- 0 WIDTH) -175)]) 
           -400
           (overlay/xy
            (WIND-line gift)
            -505 -33
            (overlay/xy
             (HP-line (gift-life1 gift))
             -97 -60          
             (overlay/xy
              (HP-line (gift-life2 gift))
              (- (- WIDTH 280)) -60
              SCENARIO)))))]

        ; Condition that prints the message "Player 2 WINS!" when
        ; the life of the first player is lower or equal to zero.
        [(and
          (<= (gift-life1 gift) 0)
          (eq? (gift-printMessage? gift) #true))
         
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
          (<= (gift-life2 gift) 0)
          (eq? (gift-printMessage? gift) #true))
         (overlay/align "center" "center"
                        (text
                         "Player 1 WINS!"
                         40 "white")
                        (overlay/align "center" "center"
                                       (rectangle 400 100 "solid" "black")
                                       SCENARIO))]

        ; This condition draws the scenario when no line is being drawn and
        ; the gift is not moving.
        ; In this case the TURN indicator, the wind and life bars are drawn
        ; over the image.
        [(and
          (eq? (gift-move gift) #false)
          (eq? (gift-line gift) #false))
         (overlay/xy
          TURN
          (cond [(eq? (gift-out gift) #true)
                 -95]

                [else
                 (- (- 0 WIDTH) -175)]) 
          -400
          (overlay/xy
           (WIND-line gift)
           -505 -33
           (overlay/xy
            (HP-line (gift-life1 gift))
            -97 -60
            (overlay/xy
             (HP-line (gift-life2 gift))
             (- (- WIDTH 280)) -60
             SCENARIO))))]

        ; In the case none of the previous conditions is met,
        ; the TURN indicator, the wind and life bars are drawn over the image.
        [else
         (overlay/xy
          TURN
          (cond [(eq? (gift-out gift) #true)
                 -95]
                [else
                 (- (- 0 WIDTH) -175)]) 
          -400
          (overlay/xy
           (WIND-line gift)
           -505 -33
           (overlay/xy
            (HP-line (gift-life1 gift))
            -97 -60
            (overlay/xy
             (HP-line (gift-life2 gift))
             (- (- WIDTH 280)) -60
             SCENARIO))))]))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; On tick function

; Signature: Structure -> Structure
; Interpretation: updates a given structure every tick according to the various
; changes in the game state.

(define (move-gift gift)
  (cond[(and
         (eq? (gift-printMessage? gift) #false)
         (eq? (gift-line gift) #false)
         (eq? (gift-move gift) #true)
         (eq? (gift-turn gift) #true))
        
        (make-gift
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)

         ; The x position of the gift is updated only if the rival player
         ; is not hit. When the rival player is hit the gift stops.
         ; The x position is updated based on the power defined by the user
         ; with the mouse interaction.
         (cond[(or
                (and
                 (hasHitP2?X (gift-giftPosX gift))
                 (hasHitP2?Y (gift-giftPosY gift)))
                
                (wall? (gift-giftPosX gift) (gift-giftPosY gift)))
               (gift-giftPosX gift)]
              [else
               (+
                (speed-x (/ (gift-power gift) 3) (gift-angle gift))
                (gift-giftPosX gift))])

         ; The y position of the gift is updated only if the rival player or the wall
         ; is not hit. When the rival player or wall is hit the gift stops.
         ; The y position is updated according to the external function
         ; trajectory that computes the exact parabola followed by the gift
         ; during its fly. The trajectory is also influenced by the wind.
         (cond [(or
                 (and
                  (hasHitP2?X (gift-giftPosX gift))
                  (hasHitP2?Y (gift-giftPosY gift)))
                
                 (wall? (gift-giftPosX gift) (gift-giftPosY gift)))
                (gift-giftPosY gift)]
               [else

                (cond [(>= (gift-wind gift) 0)
                       (trajectory (velocity (+ (gift-power gift) (* (/ (gift-wind gift) 10)(gift-power gift))) (gift-angle gift)) 100 180 (gift-angle gift) (gift-giftPosX gift))]
                
                      [else
                       (trajectory (velocity (+ (gift-power gift) (* (/ (gift-wind gift) 10)(gift-power gift))) (gift-angle gift))  100 180 (gift-angle gift) (gift-giftPosX gift))
                       ])])

         ; The rotation of the gift is updated while moving.
         (- (gift-rot gift) (* (rot gift) 400))


         ; Change the turn when the wall is hit by the gift.
         (cond [(wall? (gift-giftPosX gift)(gift-giftPosY gift))
                (gift-turn gift)]
               [else (gift-turn gift)])

         (gift-line gift)
         (gift-angle gift)
         (gift-power gift)
         (gift-life1 gift)

         ; Defining the damage inflicted by the gift on the player.
         
         ; When the head is hit 50 of 100 life points are deducted by the
         ; current life of the player.
         (cond[(and
                (hasHitP2?Y (gift-giftPosY gift))
                (hasHitP2?X (gift-giftPosX gift))
                (and
                 (<= (gift-giftPosY gift) 200)
                 (> (gift-giftPosY gift) 135)))
               (- (gift-life2 gift) 50)]

              ; When the chest is hit 30 of 100 life points are deducted by the
              ; current life of the player.
              [(and
                (hasHitP2?Y (gift-giftPosY gift))
                (hasHitP2?X (gift-giftPosX gift)) 
                (and
                 (<= (gift-giftPosY gift) 135)
                 (> (gift-giftPosY gift) 78)))
               (- (gift-life2 gift) 30)]

              ; When the lower body is hit, 20 of 100 life points are deducted
              ; by the current life of the player.
              [(and
                (hasHitP2?Y (gift-giftPosY gift))
                (hasHitP2?X (gift-giftPosX gift))
                (and
                 (<= (gift-giftPosY gift) 78)
                 (>= (gift-giftPosY gift) 50)))
               (- (gift-life2 gift) 20)]

              ; In other cases the life is not changed.
              [else
               (gift-life2 gift)])

         ; When the player or the wall is hit by the gift the gift stops.
         ; If no player or wall is hit the gift keeps moving until it is out of
         ; the scene.

         (cond[(or
                (and
                 (hasHitP2?X (gift-giftPosX gift))
                 (hasHitP2?Y (gift-giftPosY gift)))
                 
                (wall? (gift-giftPosX gift)(gift-giftPosY gift)))
               #false]
              [else
               #true])
         #false
         #false
         (gift-wind gift)
         (gift-out gift))
        ]

       ;;;;;;;;

       ; When there is no message printed and no line being drawn and the
       ; gift is moving on the scene and it's player two's turn.

       [(and
         (eq? (gift-line gift) #false)
         (eq? (gift-printMessage? gift) #false)
         (eq? (gift-move gift) #true)
         (eq? (gift-turn gift) #false))
        
        (make-gift
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)

         ; The x position of the gift is updated only if the rival player or the wall
         ; is not hit. When the rival player or the wall is hit the gift stops.
         ; The x position is updated based on the power defined by the user
         ; with the mouse interaction.

         (cond [(or
                 (and
                  (hasHitP2?X (gift-giftPosX gift))
                  (hasHitP2?Y (gift-giftPosY gift)))
                 (wall? (gift-giftPosX gift)(gift-giftPosY gift)))
                (gift-giftPosX gift)]
               [else
                (+ (speed-x (/ (gift-power gift) 3)
                            (gift-angle gift))
                   (gift-giftPosX gift))])

         ; The y position of the gift is updated only if the rival player or the wall
         ; is not hit. When the rival player or the wall is hit the gift stops.
         ; The y position is updated according to the external function
         ; trajectory that computes the exact parabola followed by the gift
         ; during its fly. The trajectory is also influenced by the wind.
         (cond [(or
                 (and
                  (hasHitP2?X (gift-giftPosX gift))
                  (hasHitP2?Y (gift-giftPosY gift)))
                 (wall? (gift-giftPosX gift)(gift-giftPosY gift)))
                (gift-giftPosY gift)]
               [else
                (cond [(>= (gift-wind gift) 0)
                       (trajectory
                        (velocity (- (gift-power gift)
                                     (* (/ (gift-wind gift) 10)(gift-power gift)))
                                  (gift-angle gift))
                        100 180 (gift-angle gift) (gift-giftPosX gift))]
                
                      [else
                       (trajectory
                        (velocity (- (gift-power gift)
                                     (* (/ (gift-wind gift) 10)(gift-power gift)))
                                  (gift-angle gift))
                        100 180 (gift-angle gift) (gift-giftPosX gift))])])

         ; The rotation of the gift is updated while moving.
         (-  (gift-rot gift)  (* (rot gift) 400))

         ; Change the turn when the wall is hit by the gift.
         (cond [(wall? (gift-giftPosX gift)(gift-giftPosY gift))
                (gift-turn gift)]
               [else (gift-turn gift)])

         (gift-line gift)
         (gift-angle gift)
         (gift-power gift)

         ; Defining the damage inflicted by the gift on the player.
         
         ; When the head is hit 50 of 100 life points are deducted by the
         ; current life of the player.
         (cond[(and
                (hasHitP2?Y (gift-giftPosY gift))
                (hasHitP2?X (gift-giftPosX gift))
                (and
                 (<= (gift-giftPosY gift) 200)
                 (> (gift-giftPosY gift) 135)))
               (- (gift-life1 gift) 50)]

              ; When the chest is hit 30 of 100 life points are deducted by the
              ; current life of the player.
              [(and
                (hasHitP2?Y (gift-giftPosY gift))
                (hasHitP2?X (gift-giftPosX gift)) 
                (and
                 (<= (gift-giftPosY gift) 135)
                 (> (gift-giftPosY gift) 78)))
               (- (gift-life1 gift) 30)]

              ; When the lower body is hit, 20 of 100 life points are deducted
              ; by the current life of the player.
              [(and
                (hasHitP2?Y (gift-giftPosY gift))
                (hasHitP2?X (gift-giftPosX gift))
                (and
                 (<= (gift-giftPosY gift) 78)
                 (>= (gift-giftPosY gift) 50)))
               (- (gift-life1 gift) 20)]

              ; In other cases the life is not changed.
              [else
               (gift-life1 gift)])
         
         (gift-life2 gift)

         ; When the player is hit by the gift the gift stops.
         ; If no player is hit the gift keeps moving until it is out of
         ; the scene.
         (cond [(or
                 (and
                  (hasHitP2?X (gift-giftPosX gift))
                  (hasHitP2?Y (gift-giftPosY gift)))
                 (wall? (gift-giftPosX gift)(gift-giftPosY gift)))
                #false]
               [else
                #true])
         #false
         #false
         (gift-wind gift)
         (gift-out gift))]


       ;;;;;;;

       ; When it's first player's turn and the gift
       ; goes out of the scene without hitting any player or wall.
       
       [(and
         (eq? (gift-turn gift) #true)
         (eq? (gift-printMessage? gift) #false)
         (wall? (gift-giftPosX gift)(gift-giftPosY gift)))


        ; The structure is redefined, turn changed and
        ; the starting position of the gift is also restored.
        ; A new random value of wind is created.

        (make-gift
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)
         130
         350
         180
         #false
         (gift-line gift)
         (gift-angle gift)
         (gift-power gift)
         (gift-life1 gift)
         (gift-life2 gift)
         #false
         #false
         #false
         (/ (* 10 (random -10 10)) 100)
         (gift-out gift))]

       ;;;;;;
       
       ; When it's first player's turn and the gift
       ; goes out of the scene without hitting any player.
       [(and
         (eq? (gift-turn gift) #false)
         (eq? (gift-printMessage? gift) #false)
         (wall? (gift-giftPosX gift)(gift-giftPosY gift)))


        ; The structure is redefined, turn changed and
        ; the starting position of the gift is also restored.
        ; A new random value of wind is created.   
        (make-gift 
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)
         130
         350
         180
         #true
         (gift-line gift)
         (gift-angle gift)
         (gift-power gift)
         (gift-life1 gift)
         (gift-life2 gift)
         #false
         #false
         #false
         (/ (* 10 (random -10 10)) 100)
         (gift-out gift))]
       
       ; When one of the two players' life is equal or lower than zero a
       ; message with the winner is displayed.
       [(and
         (eq? (gift-turn gift) #true)
         (eq? (gift-printMessage? gift) #false)
         (or
          (isOut? (gift-giftPosX gift))
          (hasHitGround? (gift-giftPosY gift))
          (< (gift-giftPosY gift) 0)
          (and
           (hasHitP2?X (gift-giftPosX gift))
           (hasHitP2?Y (gift-giftPosY gift)))
          (wall? (gift-giftPosX gift)(gift-giftPosY gift))))


        
        ; The structure is redefined, turn changed and
        ; the starting position of the gift is also restored.
        ; A new random value of wind is created.
        
        (make-gift
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)
         130
         350
         180
         #false
         (gift-line gift)
         (gift-angle gift)
         (gift-power gift)
         (gift-life1 gift)
         (gift-life2 gift)
         #false
         #false
         #false
         (/ (* 10 (random -10 10)) 100)
         (gift-out gift))]


       ;;;;;;

       ; When it's first player's turn and the gift
       ; goes out of the scene without hitting any player.

       [(and
         (eq? (gift-turn gift) #false)
         (eq? (gift-printMessage? gift) #false) 
         (or
          (isOut? (gift-giftPosX gift))
          (hasHitGround? (gift-giftPosY gift))
          (< (gift-giftPosY gift) 0)
          (and
           (hasHitP2?X (gift-giftPosX gift))
           (hasHitP2?Y (gift-giftPosY gift)))
          (wall? (gift-giftPosX gift)(gift-giftPosY gift))))

        ; The structure is redefined, turn changed and
        ; the starting position of the gift is also restored.
        ; A new random value of wind is created.
        
        (make-gift
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)
         130
         350
         0
         #true
         (gift-line gift)
         (gift-angle gift)
         (gift-power gift)
         (gift-life1 gift)
         (gift-life2 gift)
         #false
         #false
         #false
         (/ (* 10 (random -10 10)) 100)
         (gift-out gift))
        ]

       
       ; When one of the two players' life is equal or lower than zero a
       ; message with the winner is displayed.
       
       [(and
         (eq? (gift-printMessage? gift) #false)
         (or
          (eq? (gift-turn gift) #false)
          (eq? (gift-turn gift) #true))
         (or
          (<= (gift-life1 gift) 0)
          (<= (gift-life2 gift) 0)))
        (make-gift
         (gift-mx1 gift)
         (gift-my1 gift)
         (gift-mx2 gift)
         (gift-my2 gift)
         130
         350
         0
         #true
         #false
         (gift-angle gift)
         (gift-power gift)
         (gift-life1 gift)
         (gift-life2 gift)
         #false
         #true
         #false
         (gift-wind gift)
         (gift-out gift))]

       ; In any other case the unmodified struct is given to the
       ; big bang.
        
       [else gift]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (initial-velocity gift)
  (* (/ (gift-power gift) 100) (* WIDTH 10)))

; Signature: Num -> Num
; Interpretation: given an angle in degrees, returns the angle in
; radiants.

(define (degrees->radiant degrees)
  (/ degrees 57.2958))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: Structure -> Num
; Interpretation: given a structure, returns the angle of rotation
; of the gift.

(define (rot gift)
  (+ 180 (/ (gift-angle gift)
            (/ (* (distance (gift-power gift)
                            (cond [(< (gift-angle gift) 0) 1]
                                  [else
                                   (gift-angle gift)])) 10)
               1))))
  
(check-expect (rot initialState) 180)


; Signature: Num Num -> Num
; Interpretation: given a power and an angle, returns the range of the
; gift (in pixels).
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
  (sqrt (+
         (* (speed-x power alpha)(speed-x power alpha))
         (* (speed-y power alpha)(speed-y power alpha)))))

(check-within (velocity 100 15) 100.0 0.001)
(check-within (velocity 50 15) 50.0 0.001)

; Signature: Num Num Num Num Num -> Num
; Interpretation: given an initial velocity, an x and y coordinate
; occupied by the gift, and angle and an x, returns the respective y
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Signature: Structure -> Num
; Interpretation: given a structure returns the length of the segment formed
; by the two coordinates of the mouse. In other words, it defines the length
; of the segment drawn with the mouse by the user. In the case the segment is
; bigger than 100 px, the value is limited to 100 px.

(define (power gift) 
  (cond [(<
          (distance-two-points
           (gift-mx1 gift) (gift-my1 gift)
           (gift-mx2 gift) (gift-my2 gift)) 200)
         (ceiling (/ (distance-two-points
                      (gift-mx1 gift) (gift-my1 gift)
                      (gift-mx2 gift) (gift-my2 gift)) 2))]
        [(>= (distance-two-points
              (gift-mx1 gift) (gift-my1 gift)
              (gift-mx2 gift) (gift-my2 gift)) 200)
         100]))

(check-expect (power initialState) 0) 

; Signature: Number -> Number
; Given the range of the gift, return the x coordinate of the vertex of the parabola.

(define (vertex-x distance) 
  (/ distance 2))

; Signature: Number -> Number
; Given the speed and angle of the gift, return the y coordinate of the vertex of the parabola.

(define (vertex-y power alpha)
  (/ (speed-y power alpha) (* 2 G)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





; Mouse handler
; Signature: Structure Int Int mouse-event -> Structure
; Interpretation: updates the world state after a mouse event.


(define (mouse-handler gift x y mouse-event)

  ; When the button-down event happens the coordinates of the mouse are saved in the struct.

  (cond [(and
          (eq? mouse-event "button-down")
          (eq? (gift-printMessage? gift) #false))
         (make-gift
          x y x y
          (gift-giftPosX gift)
          (gift-giftPosY gift)
          (gift-rot gift) (gift-turn gift) #true
          (gift-angle gift) (gift-power gift)
          (gift-life1 gift) (gift-life2 gift)
          (gift-move gift)
          (gift-printMessage? gift)
          (gift-shouldStop? gift)
          (gift-wind gift)
          (gift-out gift))]

        ; During the drag event the coordinates of the mouse are saved in the struct.

        [(and
          (eq? mouse-event "drag")
          (eq? (gift-printMessage? gift) #false))
         (make-gift
          (gift-mx1 gift) (gift-my1 gift)
          x y
          (gift-giftPosX gift)
          (gift-giftPosY gift)


          
          
          (cond
            [(or
              (eq? (gift-mx1 gift) x)
              (eq? (gift-my1 gift) y))
             (alpha
              (distance-two-points
               (gift-mx1 gift) (gift-my1 gift)
               (+ x 0.01) (+ y 0.01))
              (abs (- (gift-mx1 gift) (+ x 0.01)))
              (- (gift-my1 gift) (+ y 0.01)))]
            [else
             (alpha
              (distance-two-points
               (gift-mx1 gift) (gift-my1 gift)
               x y)
              (abs (- (gift-mx1 gift) x))
              (- (gift-my1 gift) y))])

          

          (gift-turn gift) #true

          ; The angle is computed based on the segment drawn with the mouse.
          (cond
            [(or
              (eq? (gift-mx1 gift) x)
              (eq? (gift-my1 gift) y))
             (alpha
              (distance-two-points
               (gift-mx1 gift) (gift-my1 gift)
               (+ x 0.01) (+ y 0.01))
              (abs (- (gift-mx1 gift) (+ x 0.01)))
              (- (gift-my1 gift) (+ y 0.01)))]
            [else
             (alpha
              (distance-two-points
               (gift-mx1 gift) (gift-my1 gift)
               x y)
              (abs (- (gift-mx1 gift) x))
              (- (gift-my1 gift) y))])
          
          (power gift)
          (gift-life1 gift) (gift-life2 gift)
          (gift-move gift)
          (gift-printMessage? gift)
          (gift-shouldStop? gift)
          (gift-wind gift)
          (gift-out gift))
         ]

        ; When the mouse is released, the big bang starts to draw
        ; the moving gift.

        [(and
          (eq? mouse-event "button-up")
          (eq? (gift-printMessage? gift) #false))
         (make-gift
          (gift-mx1 gift) (gift-my1 gift)
          (gift-mx1 gift) (gift-my1 gift)
          (gift-giftPosX gift)
          (gift-giftPosY gift)
          (gift-rot gift) (gift-turn gift) #false
          (gift-angle gift) (gift-power gift)
          (gift-life1 gift) (gift-life2 gift)
          #true
          #false
          #false
          (gift-wind gift)
          (cond[(and
                 (eq? (gift-out gift) #true)
                 (eq? (gift-move gift) #false))
                #false]
               [(and
                 (eq? (gift-out gift) #false)
                 (eq? (gift-move gift) #false))
                #true]
               [else (gift-out gift)]))]

        ; If the mouse is clicked, dragged or released
        ; while the gift is moving the game state is not
        ; influenced.
        
        [(and
          (eq? mouse-event "button-down")
          (eq? (gift-move gift) #true)
          (or
           (eq? (gift-turn gift) #true)
           (eq? (gift-turn gift) #false)))
         gift]

        [(and
          (eq? mouse-event "drag")
          (eq? (gift-move gift) #true)
          (or
           (eq? (gift-turn gift) #true)
           (eq? (gift-turn gift) #false)))
         gift]
        
        [(and
          (eq? mouse-event "button-up")
          (eq? (gift-move gift) #true)
          (or
           (eq? (gift-turn gift) #true)
           (eq? (gift-turn gift) #false)))
         gift]

        ; When the message with the winner is printed,
        ; the mouse actions do not influence the scene.
        
        [(and
          (or
           (eq? mouse-event "button-up")
           (eq? mouse-event "button-down"))
          (eq? (gift-printMessage? gift) #true))
         gift]

         ; In any other case the current game state is returned.

        [else gift]))

; Signature: Structure Key -> Structure
; Interpretation: for a given keyboard event return the new state of
; the game.
; When "r" is clicked the initial game state is applied (restart).
; When "q" is clicked the big-bang is closed.
(define (key-handler gift key)
  (cond
    [(string=? key "r")
     initialState]
    [(string=? key "q")
     (make-gift
      (gift-mx1 gift)
      (gift-my1 gift)
      (gift-mx2 gift)
      (gift-my2 gift)
      130
      350
      0
      (gift-turn gift)
      #false
      (gift-angle gift)
      (gift-power gift)
      (gift-life1 gift)
      (gift-life2 gift)
      #false
      #true
      #true
      (gift-wind gift)
      (gift-out gift))]
   
    [else gift]))

; Signature: Structure -> Boolean
; Interpretation: for a given game state returns true if the big-bang should be closed.
(define (shouldStop? gift)
  (cond [(eq? (gift-shouldStop? gift) #true)
         #true]
        [else
         #false])) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Defining the function christmasrun that will be called in the main menu.
; Discussed with TA Long.

(define (christmasrun a)
  (big-bang initialState
            [to-draw draw-scene]
            [on-tick move-gift 0.0025]
            [display-mode 'normal]
            [on-key key-handler]
            [stop-when shouldStop?]
            [close-on-stop #true]
            [on-mouse mouse-handler]))
