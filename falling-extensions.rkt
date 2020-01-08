;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname falling-extensions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname falling-extensions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
The goal of this assignment is to extend your falling
game.
In addition to the extensions listed below, find all
opportunities to abstract the functions that you have
already written using map, filter, and other such
higher-order functions.
1) make objects that would be touching the paddle, if
   they were at the bottom of the screen, look different.
   That is, help the player to understand the extent of the
   paddle with a subtly different faller image. (This
   applies to the new types of fallers you add for the
   second part of the homework too.)
2) make a new type of faller, such that, when it touches
   the paddle, the paddle gets wider and another such that,
   when it touches the paddle, the paddle gets narrower.
   These fallers should appear at regular intervals (not
   randomly) in your game. For example, every 10th faller
   could be a shrinking faller, say.
In order to avoid being overwhelmed by the changes to
the game, first be sure you have a good test suite for
your existing game. Second, pick only one small aspect
of the above bullets to work on first. Break the changes
up into two phases: a refactoring phase and then a
semantics-breaking change. That is, first change the
program in such a way that the game plays the same as it
played before but that prepares you for the new game's
behavior. Once that change is implemented and tested,
then change the behavior in a second (usually much
easier) step. At each stage, make sure you have a
complete set of passing tests and, even better, check
each change in to git so you can exploit the 'git diff'
command.
Note that changing the data definition of Fw-World is a
good idea. Think carefully about the changes, though,
and don't be afraid to change your mind about what the
best changes are.
Note that your final submission should be consistent,
(i.e., make sure your signatures, templates, tests,
bodies, etc etc are all matched up as if they had been
written from scratch with the final data definitions you
end up using).
|#

;
;
;                                                      ;
;     ;;;;                                             ;
;   ;;    ;
;   ;       ;    ;  ; ;;;    ;;;;   ; ;;;    ;;;;    ;;;     ;;;;
;   ;        ;   ;  ;;   ;  ;;  ;;  ;;  ;   ;    ;     ;    ;    ;
;    ;;      ;  ;   ;    ;  ;    ;  ;    ;  ;          ;    ;
;      ;;;   ;  ;   ;    ;  ;    ;  ;    ;  ;;;        ;    ;;;
;         ;   ; ;   ;    ;  ;    ;  ;    ;     ;;;     ;       ;;;
;         ;   ;;    ;    ;  ;    ;  ;    ;       ;     ;         ;
;   ;    ;;   ;;    ;    ;  ;;  ;;  ;;  ;   ;    ;     ;    ;    ;
;    ;;;;;     ;    ;    ;   ;;;;   ; ;;;    ;;;;   ;;;;;;;  ;;;;
;              ;                    ;
;             ;                     ;
;            ;;                     ;
;

#|
For this exercise you will design and implement a
minimalistic, one finger input game. The player
controls a paddle that moves back and forth at the
bottom of the screen. Falling from the heavens are
some items that you're trying to capture on your
paddle. The paddle never stays still; it
continuously moves left and right along the bottom
of the screen.
There is only a single kind of input accepted
(think like a thumb tap on a phone); the tap
reverses the direction of the paddle. That is, if
there is no input, then the paddle moves from the
left edge to the right edge and then back to the
left edge, over and over. When the user taps, then
the paddle reverses direction even when it isn’t
at one of the edges. So, if the user wishes to
keep the paddle in one spot, they can tap
repeatedly.
The player gets 10 points for each falling item
that the paddle catches and loses one point each
time they tap to reverse direction, but the score
never goes below zero.
Use the world data definition given below; note
that there is some ambiguity in this definition.
For example, do the `Posn`s of the fallers
represent their centers or upper-left corners? You
will need to figure out issues like this one and
make sure your code is consistent.
Either way, you should use the center of the
faller to determine if it has fallen off of the
bottom or if it has hit the paddle.
|#

(require 2htdp/image)
(require 2htdp/universe)


;
;
;
;   ;;;;;             ;
;   ;    ;            ;
;   ;     ;   ;;;   ;;;;;;    ;;;
;   ;     ;  ;   ;    ;      ;   ;
;   ;     ;      ;    ;          ;
;   ;     ;  ;;;;;    ;      ;;;;;
;   ;     ; ;    ;    ;     ;    ;
;   ;     ; ;    ;    ;     ;    ;
;   ;    ;  ;   ;;    ;     ;   ;;
;   ;;;;;    ;;; ;     ;;;   ;;; ;
;
;
;
;

; A Faller-world is
;   (make-fw Number Number Direction List-of-Posn Natural)
; interp.: if `a-fw` is a Faller-world then all of:
; - (fw-paddle a-fw) is the x coordinate of the paddle,
; - (fw-paddle-width a-fw) is the width of the paddle,
; - (fw-direction a-fw) gives which direction the paddle is moving,
; - (fw-fallers a-fw) is a list of the positions of the fallers, and
; - (fw-score a-fw) is the score.
(define-struct fw (paddle paddle-width direction fallers score))

;A Faller is
;   (make-faller x-posn y-posn Type)
; Type is one of: 1, 2, 3
; 1 = regular
; 2 = enlarge
; 3 = shrink
(define-struct faller (x y type))
(define REGULAR-TYPE 1)
(define ENLARGE-TYPE 2)
(define SHRINK-TYPE 3)

(define a-fw (make-fw 0
                      20
                      "right"
                      (list (make-faller 0 0 1)
                            (make-faller 20 0 1)
                            (make-faller 30 5 2))
                      0))
(define another-fw (make-fw 0 20 "right" (list (make-faller 0 0 1)) 0))
(define third-fw (make-fw 0 20 "right" (list (make-faller 0 0 1)
                                             (make-faller 10 0 2)) 0))


;
;
;
;     ;;;;                            ;                       ;
;    ;    ;                           ;                       ;
;   ;        ;;;;   ; ;;;    ;;;;   ;;;;;;    ;;;   ; ;;;   ;;;;;;   ;;;;
;   ;       ;;  ;;  ;;   ;  ;    ;    ;      ;   ;  ;;   ;    ;     ;    ;
;   ;       ;    ;  ;    ;  ;         ;          ;  ;    ;    ;     ;
;   ;       ;    ;  ;    ;  ;;;       ;      ;;;;;  ;    ;    ;     ;;;
;   ;       ;    ;  ;    ;     ;;;    ;     ;    ;  ;    ;    ;        ;;;
;   ;       ;    ;  ;    ;       ;    ;     ;    ;  ;    ;    ;          ;
;    ;    ; ;;  ;;  ;    ;  ;    ;    ;     ;   ;;  ;    ;    ;     ;    ;
;     ;;;;   ;;;;   ;    ;   ;;;;      ;;;   ;;; ;  ;    ;     ;;;   ;;;;
;
;
;
;

;; You will use these named constants in the
;; definitions of your functions to determine the
;; world’s dimensions and when fallers are created.
;; Your program should still work—with no other
;; changes—when these constants are adjusted (within
;; a reasonable range).
(define WORLD-WIDTH 200)   ; window width
(define WORLD-HEIGHT 300)  ; window height
(define MAX-FALLERS 20)    ; maximum faller count
(define INV-P-FALLER 25)   ; inverse of per-tick probability of new faller 
(define PADDLE-WIDTH 50)
(define PADDLE-HEIGHT 12)
(define FALLER-WIDTH 20)
(define FALLER-HEIGHT 20)
(define EMPTY-SCENE (empty-scene (+ 0 WORLD-WIDTH) (+ 50 WORLD-HEIGHT)))

(define (change-paddle-image World)
  (rectangle (fw-paddle-width World) 12 "solid" "black"))

(define FALLER-IMAGE (circle 10 "solid" "blue"))  
(define TARGET-FALLER-IMAGE (circle 10 "solid" "red"))  
(define PADDLE-IMAGE (rectangle 50 12 "solid" "black"))  
(define ENLARGE-FALLER-IMAGE (rectangle 20 20 "solid" "green"))
(define TARGET-ENLARGE-FALLER-IMAGE (rectangle 20 20 "solid" "red"))
(define SHRINK-FALLER-IMAGE (triangle 20 "solid" "yellow"))
(define TARGET-SHRINK-FALLER-IMAGE (triangle 20 "solid" "red"))

#|
For the first step, give your game some flavor.
Find or design an image to show as the falling
items and design an image to use as the paddle.
For the paddle, use `2htdp/image` to make an
image, but for the fallers you may find an image
online to include in your program (or you may
compose your own one using `2htdp/image`).
Make your falling image about 20 pixels tall and
20 pixels wide and make your paddle about 12
pixels tall and 50 pixels wide. Use `image-width`
and `image-height` to confirm the sizes.
Please DO NOT paste the image that you find
directly into your code because that makes version
control (Git) not work very well on the resulting
file. Instead, you should save the image as a file
in this directory and load it in your program
using the `bitmap/file` function. For example, if
you save your faller image as `faller.jpg` (in the
same directory as this file), then you can load it
like this:
  (define FALLER-IMAGE (bitmap/file "faller.jpg"))
In order to a new file like `faller.jpg` to be
committed to Git and uploaded to GitHub (so that
we can see it when grading), you need to use the
`git add` command, like so:
  $ git add faller.jpg
When you commit after `git add`, the file that you
added will be included in the commit.
|#


;
;
;                                              ;
;    ;;;;;;                           ;        ;
;    ;                                ;
;    ;      ;    ;  ; ;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;
;    ;      ;    ;  ;;   ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ;
;    ;;;;;  ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;
;    ;      ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;;;
;    ;      ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;     ;;;
;    ;      ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;       ;
;    ;      ;   ;;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ;
;    ;       ;;; ;  ;    ;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;   ;;;;
;
;
;
;
;draw : Faller-world -> image
;to draw the scene given world
;;on the image with fallers, draw paddle

; Examples:
(check-expect (draw a-fw)
              (place-image (score-board 0) 50 (+ WORLD-HEIGHT 30)
                           (place-image (change-paddle-image a-fw)
                                        (fw-paddle a-fw) WORLD-HEIGHT
                                        (draw-fallers a-fw))))
;;strategy: function composition
(define (draw World)
  (place-image  (score-board (fw-score World))
                50 (+ WORLD-HEIGHT 30)
                (place-image (change-paddle-image World)
                             (fw-paddle World) WORLD-HEIGHT
                             (draw-fallers World))))
              

;draw-fallers: FallerWorld -> image
;to recursively draw fallers in list

; Examples:
(check-expect (draw-fallers another-fw)
              (place-image TARGET-FALLER-IMAGE 0 0 EMPTY-SCENE))
(check-expect (draw-fallers third-fw)
              (place-image TARGET-FALLER-IMAGE 0 0
                           (place-image TARGET-ENLARGE-FALLER-IMAGE
                                        10 0 EMPTY-SCENE)))

; Strategy: structural decomposition
(define (draw-fallers world)
  (draw-faller-list (fw-fallers world)
                    (fw-paddle world)
                    (fw-paddle-width world)
                    EMPTY-SCENE))


; draw-fallers-list: [Listof Faller] Image Number Number -> Image
; to recursively draw fallers in list

; Examples:
(check-expect (draw-faller-list (list (make-faller 0 0 1)
                                      (make-faller 10 0 1))
                                0 20 EMPTY-SCENE)
              (place-image TARGET-FALLER-IMAGE 0 0
                           (place-image TARGET-FALLER-IMAGE
                                        10 0 EMPTY-SCENE)))
(check-expect (draw-faller-list (list (make-faller 0 0 2)
                                      (make-faller 10 0 3))
                                0 20 EMPTY-SCENE)
              (place-image TARGET-ENLARGE-FALLER-IMAGE 0 0
                           (place-image TARGET-SHRINK-FALLER-IMAGE
                                        10 0 EMPTY-SCENE)))

; Strategy: Structural Decomposition
(define (draw-faller-list fallers  paddle-position paddle-width background)
  (cond
    [(empty? fallers)  background]
    [else
     (draw-one-faller
      (first fallers)
      (draw-faller-list (rest fallers)  paddle-position paddle-width background)
      paddle-position paddle-width)]))


; draw-one-faller: Faller Image Number Number -> Image
; to draw the given faller into the background

; Examples:
(check-expect (draw-one-faller (make-faller 0 10 2) EMPTY-SCENE 0 20)
              (place-image TARGET-ENLARGE-FALLER-IMAGE 0 10 EMPTY-SCENE))
(check-expect (draw-one-faller (make-faller 40 10 1) EMPTY-SCENE 0 20)
              (place-image FALLER-IMAGE 40 10 EMPTY-SCENE))
(check-expect (draw-one-faller (make-faller 40 10 2) EMPTY-SCENE 0 20)
              (place-image ENLARGE-FALLER-IMAGE 40 10 EMPTY-SCENE))
(check-expect (draw-one-faller (make-faller 40 10 3) EMPTY-SCENE 0 20)
              (place-image SHRINK-FALLER-IMAGE 40 10 EMPTY-SCENE))


;get-faller-image: faller-type boolean -> image
;returns the corresponding image for the faller
(define (get-faller-image type boolean)
  (cond
    [(and (= type REGULAR-TYPE) boolean) TARGET-FALLER-IMAGE]
    [(and (= type REGULAR-TYPE) (not boolean)) FALLER-IMAGE]
    [(and (= type ENLARGE-TYPE) boolean) TARGET-ENLARGE-FALLER-IMAGE]
    [(and (= type ENLARGE-TYPE) (not boolean)) ENLARGE-FALLER-IMAGE]
    [(and (= type SHRINK-TYPE) boolean) TARGET-SHRINK-FALLER-IMAGE]
    [(and (= type SHRINK-TYPE) (not boolean)) SHRINK-FALLER-IMAGE]))


; Strategy: Structural Decomposition
(define (draw-one-faller faller background paddle-position paddle-width)
  (place-image (get-faller-image (faller-type faller)
                                 (atPaddleWidth? faller
                                                 paddle-position
                                                 paddle-width))
               (faller-x faller)
               (faller-y faller)
               background))
;  (cond
;    [(= (faller-type faller) 2) ;enlarge
;     (if (atPaddleWidth? faller paddle-position paddle-width)
;         (place-image TARGET-ENLARGE-FALLER-IMAGE
;                      (faller-x faller)
;                      (faller-y faller)
;                      background)
;         (place-image ENLARGE-FALLER-IMAGE
;                      (faller-x faller)
;                      (faller-y faller)
;                      background))]
;    [(= (faller-type faller) 3) ;shrink
;     (if (atPaddleWidth? faller paddle-position paddle-width)
;         (place-image TARGET-SHRINK-FALLER-IMAGE
;                      (faller-x faller)
;                      (faller-y faller)
;                      background)
;         (place-image SHRINK-FALLER-IMAGE
;                      (faller-x faller)
;                      (faller-y faller)
;                      background))]
;    [else                     ;regular
;     (if (atPaddleWidth? faller paddle-position paddle-width)
;         (place-image TARGET-FALLER-IMAGE
;                      (faller-x faller)
;                      (faller-y faller)
;                      background)
;         (place-image FALLER-IMAGE
;                      (faller-x faller)
;                      (faller-y faller)
;                      background))]))


; score-board: Number -> Image
; takes a score and displays it
(define (score-board score)
  (text (string-append "Score: " (number->string score)) 24 "black"))
  

; A Direction is one of:
; - "left"
; - "right"

; A List-of-Posn is one of: --> List of struct with coordinates for FALLERS
; - '()
; - (cons Posn List-of-Posn)
;a Position is
; (make-posn Number-x Number)
;such that Number-x and Number-y are both [0 100]
; 
;(define-struct posn (x y))

; A Posn is (make-posn Real Real) --> Coordinates
; (Note: `Real` means a real number, which excludes complex numbers.) 


;  tick : Faller-world -> Faller-world
; 1. Move all the fallers down the screen by one pixel.
; 2. If a faller touches the bottom of the screen,
; remove it from the world; if it overlaps with the
; paddle, also increment the score.
; 3. Possibly add a new faller to the list,
; starting from the top of the screen.
; 4. Move the paddle.

; Examples:
(check-expect (tick (make-fw 0 PADDLE-WIDTH "right"
                             (list (make-faller 12 12 1))
                             0))
              (make-fw 1 PADDLE-WIDTH "right" (list (make-faller 12 13 1)) 0))
(check-expect (tick (make-fw 10 PADDLE-WIDTH "left"
                             (list (make-faller 12 287 1)
                                   (make-faller 75 287 1)
                                   (make-faller 20 286 1)
                                   (make-faller 199 299 1)
                                   (make-faller 0 299 1))
                             0))
              (make-fw 9 PADDLE-WIDTH "left" (list (make-faller 12 288 1)
                                                   (make-faller 75 288 1)
                                                   (make-faller 20 287 1))
                       10))

; Strategy: function composition
(define (tick World)
  (make-fw (change-paddle-position World)
           (change-paddle-width World)
           (change-direction-boundary World)
           (maybe-add-faller
            (get-rid-fallers
             (change-faller-position (fw-fallers World)) (fw-paddle World)))
           (change-score World)))


(check-expect
 (<= 4
     (length
      (maybe-add-faller
       (list (make-faller 0 0 1)
             (make-faller 1 1 1)
             (make-faller 2 2 1)
             (make-faller 3 3 1))))
     5)
 #true)

; maybe-add-fallers: [listof faller] -> [listof faller]
; to add fallers randomly, and add special fallers on a regular basis
; Strategy: decision tree
(define (maybe-add-faller fallers)
  (cond
    [(and (< (length fallers) MAX-FALLERS)
          (zero? (random INV-P-FALLER)))
     (add-special-faller fallers)]
    [else fallers]))

; add-special-faller: [Listof faller] -> [Listof faller]
; to add special fallers at certain times

; Examples:

;(check-expect (add-special-faller 
; Strategy: Structural Decomposition
(define (add-special-faller fallers)
  (cond
    [ (and (= (remainder (length fallers) 7) 0)
           (= (remainder (length fallers) 13) 0))
      (cons (make-faller (random WORLD-WIDTH) 0 1) fallers)]
    [(= (remainder (length fallers) 7) 0)
     (cons (make-faller (random WORLD-WIDTH) 0 2) fallers)] ; at 7, add enlarge
    [ (= (remainder (length fallers) 13) 0)
      (cons (make-faller (random WORLD-WIDTH) 0 3) fallers)] ; at 13, add shrink
    [else (cons (make-faller (random WORLD-WIDTH) 0 1) fallers)]))

; change-score: World -> number
; to add 10 to score if there is a faller with (x,0)
; and paddle is at x, and return that score

; Examples:
(check-expect (change-score (make-fw 6 PADDLE-WIDTH "left" '() 0)) 0)
(check-expect (change-score (make-fw 6 PADDLE-WIDTH "left"
                                     (cons (make-faller 30 287 1)
                                           (cons (make-faller 30 30 1) '()))
                                     3))
              13)

; Strategy: Structural Decomposition
(define (change-score World)
  (+ (* 10 (filter-fallers
            (fw-fallers World)
            (fw-paddle World)
            (fw-paddle-width World)))
     (fw-score World)))


; change-paddle-width: World -> Number
; change paddle-width when different types of faller hits
; Strategy: structural decomposition
(define (change-paddle-width World)
  (- (+ (fw-paddle-width World)
        (* (filter-fallers/enlarge
            (fw-fallers World)
            (fw-paddle World)
            (fw-paddle-width World))
           10))
     (* (filter-fallers/shrink (fw-fallers World)
                               (fw-paddle World)
                               (fw-paddle-width World))
        20)))

;hits/enlarge: faller->boolean
;checks if the enlarge-faller (type=3) hits the paddle
(define (hits?/enlarge faller paddle-x paddle-width)
  (and (atPaddleHeight? faller PADDLE-HEIGHT)
       (atPaddleWidth? faller paddle-x paddle-width)
       (=(faller-type faller) ENLARGE-TYPE)))

;hits/shrink: faller->boolean
;checks if the shrink-faller (type=3) hits the paddle
(define (hits?/shrink faller paddle-x paddle-width)
  (and (atPaddleHeight? faller PADDLE-HEIGHT)
       (atPaddleWidth? faller paddle-x paddle-width)
       (=(faller-type faller) SHRINK-TYPE)))

; filter-fallers/enlarge: [listof faller] number number -> number
; to return the length of the filtered list
; (length of the [list-of (fallers-type=2) that hit the paddle])
(define (filter-fallers/enlarge fallers paddle-x paddle-width)
  (local [(define (keep? faller)
            (hits?/enlarge faller paddle-x paddle-width))]
    (length (filter keep? fallers))))

; filter-fallers/shrink [listof faller] number number -> number
; to return the length of the filtered list
; (length of the [list-of (fallers-type=3) that hit the paddle])
(define (filter-fallers/shrink fallers paddle-x paddle-width)
  (local [(define (keep? faller)
            (hits?/shrink faller paddle-x paddle-width))]
    (length (filter keep? fallers))))


;hits?: posn Number -> boolean
;to check whether the faller hits the paddle, and keep only those that do

; Examples:
(check-expect (hits? (make-faller 0 0 3) 0 20)
              #f)

; Strategy: Structural Decomposition
(define (hits? faller paddle-x paddle-width)
  (and (atPaddleHeight? faller PADDLE-HEIGHT)
       (atPaddleWidth? faller paddle-x paddle-width)))


; filter-fallers: [listof fallers] number number -> number
; to return the length of the list of fallers that hits the paddle

; Examples:
(check-expect (filter-fallers (list (make-faller 20 20 1)
                                    (make-faller 10 10 1)
                                    (make-faller 5 287 1))
                              1
                              PADDLE-WIDTH)
              1)

; Strategy: Domain Knowledge
(define (filter-fallers fallers paddle-x paddle-width)
  (local [(define (keep? faller)
            (hits? faller paddle-x paddle-width))]
    (length(filter keep? fallers))))


;; atPaddleHeight?: faller Number -> Boolean
; to determine if the y-coordinate of faller is equal to paddle-height
(define (atPaddleHeight? posn paddle-height)
  (= (- (- WORLD-HEIGHT 1) paddle-height) (faller-y posn)))


;;atPaddleWidth?: faller Number Number -> Boolean
;to determine if the x-coordinate of faller is within the paddle-width boundary
;i.e. let a = paddle posn, X=x coordinate of posn. Then, a<=X<=a+PADDLE-WIDTH 
(define (atPaddleWidth? posn paddle-position paddle-width)
  (and (<= paddle-position (faller-x posn))
       (<= (faller-x posn) (+ paddle-position paddle-width))))

; change-fallers-position: [list of Posn] -> [list of Posn]
; to subtract 1 from the y coordinate
(define (change-faller-position list)
  (cond [(empty? list) list]
        [else    (cons (make-faller (faller-x (first list))
                                    (add1 (faller-y (first list)))
                                    (faller-type (first list)))
                       (change-faller-position (rest list)))]))
(check-expect (change-faller-position (list (make-faller 10 10 1)
                                            (make-faller 20 20 1)))
              (list (make-faller 10 11 1) (make-faller 20 21 1)))

;get-rid-fallers: [list of positions] -> [list of positions of fallers]
;to get rid of fallers at boundary (ground level... i.e WORLD-HEIGHT - 1)
; Examples:
(check-expect (get-rid-fallers (list (make-faller 10 299 1)
                                     (make-faller 20 298 1)
                                     (make-faller 12 286 1)
                                     (make-faller 12 287 1)) 10)
              (list (make-faller 20 298 1)
                    (make-faller 12 286 1)
                    (make-faller 12 287 1)))

; Strategy: Structural Decomposition
(define (get-rid-fallers list x-position)
  (cond [(empty? list) '()]
        [(<= (sub1 WORLD-HEIGHT) (faller-y (first list)))
         (get-rid-fallers (rest list) x-position)] ;; if faller at ground level
        [else (cons (first list)
                    (get-rid-fallers (rest list) x-position))]))


; change-direction-boundary: Faller-world -> String
; to change direction if at boundary

; Examples:
(check-expect (change-direction-boundary
               (make-fw 25 PADDLE-WIDTH "left" '() 0))
              "right")
(check-expect (change-direction-boundary
               (make-fw (- 199 25) PADDLE-WIDTH "right" '() 0))
              "left")
(check-expect (change-direction-boundary
               (make-fw 100 PADDLE-WIDTH "right" '() 0))
              "right")

; Strategy: Structural Decomposition
(define (change-direction-boundary World)
  (cond
    [(= (fw-paddle World) (/ (fw-paddle-width World) 2))    "right"]
    [(= (fw-paddle World)
        (- (sub1 WORLD-WIDTH)(/ (fw-paddle-width World) 2)))  "left"]
    [else (fw-direction World)]))   


; change-paddle-position: Faller-world -> x-coordinate of paddle
; to change paddle position by 1

; Examples:
(check-expect (change-paddle-position
               (make-fw 100 PADDLE-WIDTH "right" '() 0))
              101)
(check-expect (change-paddle-position
               (make-fw 100 PADDLE-WIDTH "left" '() 0))
              99)

; Structural Decomposition
(define (change-paddle-position World)
  (cond [(string=? (fw-direction World) "right")
         (add1 (fw-paddle World))]
        [else  (sub1 (fw-paddle World))]))

;  key : Faller-world Key-event -> Faller-world
; to change paddle direction when any key-event happens in game
(define (key World ke)
  (cond[(key-event? ke) (change-paddle-direction World)]
       [else World]))

;change-paddle-direction: Faller-world -> Faller-world
;to change paddle direction
(define (change-paddle-direction World)
  (cond [(string=? "left" (fw-direction World))
         (make-fw (fw-paddle World)
                  (fw-paddle-width World)
                  "right"
                  (fw-fallers World)
                  (sub1 (fw-score World)))]
        [else (make-fw (fw-paddle World)
                       (fw-paddle-width World)
                       "left"
                       (fw-fallers World)
                       (sub1 (fw-score World)))]))


#|
There are three separate pieces of the game to be
implemented: rendering the world to an image to be
shown on the screen, handling user inputs, and
adjusting the world as time passes.
Here are the sigantures of the three functions:
  draw : Faller-world -> Scene
  key : Faller-world Key-event -> Faller-world
  tick : Faller-world -> Faller-world
`draw` and `key` are fairly well-described by the
description above, but `tick` needs a little more
explanation. Conceptually, it performs several
different tasks:
 1. Move all the fallers down the screen by one
pixel.
 2. If a faller touches the bottom of the screen,
remove it from the world; if it overlaps with the
paddle, also increment the score.
 3. Possibly add a new faller to the list,
starting from the top of the screen.
 4. Move the paddle.
Be sure to compose several different functions to
accomplish these tasks, and not just one
monolithic function that does it all! (And think
carefully about how you might break up the other
two main functions, too.)
Don't forget: the coordinate system has the
origin in the upper-left, with `x` coordinates
increasing rightward and `y` coordinates
increasing *downward*.
|#


;
;
;                    ;;;
;   ;    ;             ;
;   ;    ;             ;
;   ;    ;   ;;;;      ;    ; ;;;
;   ;    ;   ;  ;;     ;    ;;  ;
;   ;;;;;;  ;    ;     ;    ;    ;
;   ;    ;  ;;;;;;     ;    ;    ;
;   ;    ;  ;          ;    ;    ;
;   ;    ;  ;          ;    ;    ;
;   ;    ;   ;         ;    ;;  ;
;   ;    ;   ;;;;;      ;;; ; ;;;
;                           ;
;                           ;
;                           ;
;

;; In your `tick` function, you need to
;; *sometimes* add a faller to the list of
;; fallers. Use a function like `maybe-add-faller`
;; (below) to (randomly) add a faller to the
;; current list. You may wish to adjust it based
;; on gameplay factors or the way you interpret
;; `Posn`s as fallers. Note that because of the
;; randomness, this function is difficult to test
;; using `check-expect`, so the test example given
;; below just checks the length of the resulting
;; list.



;; You'll use this `start` function to start your
;; faller game once you’ve completed designing the
;; three main handler functions that it depends
;; on.

; start : Any -> Faller-world
; Starts the faller game. (Argument is ignored.)
;
; Example:
;  - (start 0)
;; [remove this line to uncomment the function]
(define (start _dummy)
  (big-bang (make-fw (/ WORLD-WIDTH 2) PADDLE-WIDTH "right" '() 0)
    [on-tick tick 1/100]
    [on-key  key]
    [to-draw draw (+ WORLD-WIDTH 50) (+ 50 WORLD-HEIGHT)]))