;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname menu) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; -- THROWMAN: the game --                                             ;;;;;
;;;;; -- MAIN MENU --                                                      ;;;;;
;;;;; Developed by: Cattaneo Marco, Ferrara Salvatore, Willi Patrizia      ;;;;;
;;;;; Final project - Programming Fundamentals 1, Prof. N. Nystrom         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Importing required libraries.

(require 2htdp/image)
(require 2htdp/universe)
(require racket/base)
(require net/sendurl)

; Importing files of various game scenarios.

(require "space.rkt")
(require "medieval.rkt")
(require "christmas.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Defining the frame of the menu.

(define BACKGROUND (empty-scene 500 700 "white"))

; Importing the avatar of the developers.
(define MC (scale 0.23 (bitmap "mc.png")))
(define SF (scale 0.2 (bitmap "sf.png")))
(define PW (scale 0.22 (bitmap "pw.png")))

; abc is a Structure where:
; - x is the x pos of the mouse.
; - y is the y pos of the mouse.
; - bool is a Boolean that defines if the menu should close.

(define-struct abc [x y bool])

; Defining the initial state of the menu.
(define initialState (make-abc 0 0 #false))

; Defining the image that requests to restart the menu.
(define RESTART
  (overlay/align "center" "center"
                 (text "Restart the menu" 30 "black")
  (empty-scene 500 700 "white")))


; Defining the button for the medieval scenario
(define MEDIEVAL
  (overlay/align
   "center" "center"
   (text "MEDIEVAL" 20 "white")
   (rectangle 300 50 "solid" "DeepSkyBlue")))

; Defining the button for the space scenario
(define SPACE
  (overlay/align
   "center" "center"
   (text "SPACE" 20 "white")
   (rectangle 300 50 "solid" "DeepSkyBlue")))

; Defining the button for the christmas scenario
(define CHRISTMAS
   (overlay/align
    "center" "center"
    (above
     (text "CHRISTMAS EDITION" 20 "white")
     (text "FOR REAL PROS" 10 "white"))
    (rectangle 300 50 "solid" "DeepSkyBlue")))

; Defining the button for the how-to link.
(define HOW-TO
  (overlay/align
   "center" "center"
   (text "HOW TO PLAY" 20 "white")
   (rectangle 300 50 "solid" "DeepSkyBlue")))

; Defining the text with the main menu and game name.
(define TEXT
  (above
   (text/font "THROWMAN" 40 "red" "Gill Sans" "swiss" "normal" "bold" #false)
   (text/font "Main Menu" 25 "ForestGreen"
              #false "swiss" "normal" "normal" #false)))

; Defining the text with the credits for each one of us.

(define CREDITSMC
   (text "M. Cattaneo" 12 "ForestGreen"))

(define CREDITSSF
   (text "S. Ferrara" 12 "ForestGreen"))

(define CREDITSPW
   (text "P. Willi" 12 "ForestGreen"))



; Defining a list with all the buttons and text to be placed
; on the scene.
(define listOfButtons
  (list
   TEXT
   MEDIEVAL
   SPACE
   CHRISTMAS
   HOW-TO
   MC
   SF
   PW
   CREDITSMC
   CREDITSSF
   CREDITSPW))

; Defining a list with all the positions of the images to be placed
; on the scene.
(define buttonsPositions
  (list
   (make-posn 250 110)
   (make-posn 250 230)
   (make-posn 250 300)
   (make-posn 250 370)
   (make-posn 250 440)
   (make-posn 70 580)
   (make-posn 250 590)
   (make-posn 420 590)
   (make-posn 70 665)
   (make-posn 250 665)
   (make-posn 420 665)))

; The MENU is an image of all the text and buttons defined above.
(define MENU
  (place-images
   listOfButtons
   buttonsPositions
   BACKGROUND))

; Signature: Int Int -> Bool
; Defining if the first button is clicked.
(define (hitButton1 x y)
  (and
   (and
    (< x 425)
    (> x 75))
   (and
    (> y 205)
    (< y 255))))

; Signature: Int Int -> Bool
; Defining if the second button is clicked.
(define (hitButton2 x y)
  (and
   (and
    (< x 425)
    (> x 75))
   (and
    (> y 275)
    (< y 325))))

; Signature: Int Int -> Bool
; Defining if the third button is clicked.
(define (hitButton3 x y)
  (and
   (and
    (< x 425)
    (> x 75))
   (and
    (> y 345)
    (< y 395))))

; Signature: Int Int -> Bool
; Defining if the fourth button is clicked.
(define (hitButton4 x y)
  (and
   (and
    (< x 425)
    (> x 75))
   (and
    (> y 415)
    (< y 465))))

; To draw function
; Recalls the external scenario or web page according to the click.
(define (draw-menu abc) 
  (cond [(hitButton1 (abc-x abc) (abc-y abc))
         (medieval 1)
         RESTART]

        [(hitButton2 (abc-x abc) (abc-y abc))
         (spacerun 1)
         RESTART]
        
        [(hitButton3 (abc-x abc) (abc-y abc))
         (christmasrun 1)
         RESTART]

        [(hitButton4 (abc-x abc) (abc-y abc))
         (send-url "http://throwman.ddns.net/howtoplay.html" #f)
         RESTART]
        
        [else MENU]))

; Signature: Structure Int Int mouse-event -> Structure
; Interpretation: updates the world state based on the mouse event.

(define (handle-mouse abc x y mouse-event)
  (cond [(eq? mouse-event "button-up")
         (make-abc x y #true)]
        [else abc]))

; Signature: Structure -> Boolean
; Interpretation: for a given structure checks whether the big
; bang should close.
(define (closeMenu abc)
  (cond[(eq? (abc-bool abc) #true)
        #true]
       [else #false]))


(big-bang initialState
 [to-draw draw-menu]
 ;[stop-when closeMenu]
 [close-on-stop #false]
 [on-mouse handle-mouse])

