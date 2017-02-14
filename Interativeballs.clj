
;;http://quil.info/sketches/show/-KGNt4ML7e4ZhIwAHxfx

;; my program has 20 circles, all collide within the booundary of the canvas, there is a rectangle which can be controlled by the arrow keys
;; and mouse. If you press the up arrow key it will increase the rectangle speed, if you press down key it will decrease the speed of the rect
;; the left key will move the rectangle left, right key will move right. The rectangle is also controlled by mouse. Whereever you click mouse
;; rectangle will move there.

;; GAME POLICY
;; there is timer and maxlose point limit. you need to stop the rectangle touching the circles. if you touch the circle you keep loosing points 
;; until the rectangle touches the circle, so don't let it touch any of the circle using mouse and arrow key. If you maxlost points are less
;; than 40 points within the timer of 300 you will win the game, otherwise you loose the game. Then my program will ask the user if he wants to 
;; play the game again. If he press y it will reset everything and start the game again otherwise it will remove everything and blank screen  
;; will apppeared, user need to press Run to run it again. If the rectangle disappear from the canvas youstart the game again.
;;
;;

(ns circles.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;;; Circles Game Basic Setup
;;; CSCI 2041 Homework #7
;;; Spring 2016

;;; Constants
(def speed 5)                          ;maximm speed circles move

(def maxscore 100)

(def redraw (atom true))

(def init 300)

;---------------------------------------------------------------------
; Setup
;---------------------------------------------------------------------

(defn make-circle 
  "Creates a circle with a random color and set speed and heading."
   [x y]
  (let [angle (rand q/TWO-PI)          ;random angle
        cur-speed (+ (rand speed) 1)]  ;random speed up to our constant
       {:x x                           ;set this circle's x
    	:y y                           ;set this circle's y
        :size (+ 10 (rand 15))         ;set random diameter 
    	:color (rand 255)              ;make this colorful      
    	:speed cur-speed               ;set this circle's speed
    	:heading angle}                ;set this circle's heading
    ))                                 ;returns circle


(defn make-many-circles [n size] 
  (-> (repeatedly n #(make-circle (rand size) (rand size)))
      vec))

(defn create-rectangle [x y]
   (let [angle (rand q/TWO-PI)         
        cur-speed (+ (rand speed) 1)]
  {:x x
   :y y
   :w (+ 10 (rand 20))
   :h (+ 10 (rand 20))
   :color (rand 255)
   :heading angle
   :speed cur-speed
   :size (+ 10 (rand 20))}))

(def num (atom 0))

(def maxpoint 40)

(def timer (atom 300)) 

(defn setup 
  "Set up a sketch and return initial state."
  []
  (q/frame-rate 30)                    ;frequency update and draw functions
  (q/color-mode :hsb)                  ;how we represent colors
  (q/background 250)                           ;nice light grey color for the bg
  (q/text-font (q/create-font "calibri" 16 true))
  (let [size (q/width)
        n 20
        bg 250]
       (q/background bg)               ;nice light grey color for the bg
       ;; need to make n circles of random sizes
       ;; here we make only one circle in a list
       {:circles (vec (make-many-circles n size))
        :rec (create-rectangle 100 100)
        :running? true                 ;so we can pause and unpause in update
        :n n                           ;how many circles
        :size size                     ;how big is the sketch
        :bg bg                         ;we might want to change this later
        }))
 
;---------------------------------------------------------------------
; Update functions
;---------------------------------------------------------------------
(defn bounce-back [c size] 
 (cond (and (or (< (:x c) 0) (> (:x c) 500)) (or (< (:y c) 0) (> (:y c) 500)))
      (-> c 
      (update-in [:heading] (fn [n] (+ 3.14 (* -1 n)))))  
      (or (< (:x c) 0) (> (:x c) 500)) 
      (-> c 
      (update-in [:heading] (fn [n] (+ 3.14 (* -1 n)))))
      (or (< (:y c) 0) (> (:y c) 500))
      (-> c 
      (update-in [:heading] (fn [n] (* -1 n))))
      :else c))

(defn coord-match-helper [c x y w h]
  (let [d1 (Math/abs (- (:x c) x)) d2 (Math/abs (- (:y c) y))]
                 (if (and 
                   (< d1 (+ (/ (:size c) 2) w))
                   (< d2 (+ (/ (:size c) 2) h)))
    (swap! num inc))))

(defn move-circle [c rec state] 
  "Moves a circle according to its speed and heading"
  (let [angle (:heading c) 
        dx (* (:speed c) (q/cos(:heading c)))
        dy (* (:speed c) (q/sin(:heading c)))
        x (:x rec) y (:y rec) w (:w rec) h (:h rec)]
     (coord-match-helper c x y w h)
     (-> c 
     (update-in [:x] + dx)
     (update-in [:y] + dy)
     (bounce-back (:size c)))))

(defn move-rec [rec state]
  (let [angle (:heading rec) 
        dx (* (:speed rec) (q/cos(:heading rec)))
        dy (* (:speed rec) (q/sin(:heading rec)))]
     (-> rec
     (update-in [:x] + dx)
     (update-in [:y] + dy)
     (bounce-back (:size rec)))))

(defn update-circles 
  "Moves each circle and returns updated vector of circles."
  [circles state]
  (map (fn [c] (move-circle c (:rec state) state)) circles))

(defn update-rec
  [rec state]
  (do (if (> @timer 0) (swap! timer dec)) (move-rec rec state)))

(defn update-state 
  "Updates sketch state. If it is paused, then the state is returned unmodified."
  [state]
  (if (:running? state)
      ;add some movement and update functions so the next line moves circles
        (assoc state :circles (update-circles (:circles state) state)
        :rec (update-rec (:rec state) state) state) state))



;---------------------------------------------------------------------
; Draw functions
;---------------------------------------------------------------------

(defn draw-circle 
  "Draws an individual circle with correct color, location, and size."
  [c] 
  (q/fill (:color c) 255 255)
  (q/ellipse (:x c) (:y c) (:size c) (:size c)))

(defn draw-rectangle [rec]
  (q/fill (:color rec) 255 255)
  (q/rect (:x rec) (:y rec) (:w rec) (:h rec)))

(def step 40)
; the number of time is in contact with the ball exceeded

(defn reset []
  (if (and (= @timer 0) (< @num maxpoint)) 
  (do 
   (q/text "you win the game" 5 30) (reset! num -2000) (q/text "do you wanna play again?" 5 40))
  (do (q/text "The game is over you reached max interaction time" 5 30)
  (q/text "do you wanna play again?" 5 40))))

(defn draw-state-helper 
  "Draws the sketch state."
  [state]
  (q/background (:bg state))                    ;update the background
  (q/stroke 1)                                  ;how wide should the lines be
  (draw-rectangle (:rec state))
  (dorun (map draw-circle (:circles state)))    ;map is lazy
  
  (if (and (< @num maxpoint) (>  @timer 0))
  (do (q/text-num (deref num) 230  30)
  (q/text "lost points after interacting:" 5 30)
  (q/text-num (deref timer) 440  30) (q/text "Time Left:" 355 30))
  (reset))
  (q/fill 0))

(defn draw-state [state]
  (if @redraw (draw-state-helper state) (q/background 255)))

;---------------------------------------------------------------------
; User interaction functions
;---------------------------------------------------------------------

(defn mouse-clicked 
  "Changes background color to different shades of grey."
  [state event]
  (let [x (:x event) y (:y event)]
  (update-in state [:bg] (fn [n] (rand-int 255)))
  (update-in state [:rec]
  (fn [rec](merge rec {:x (:x event) :y (:y event)})))))

(defn increase [rec]
  (let [speed (:speed rec)]
  (update-in rec [:speed] (fn [n] (max 1.0 (+ n 0.25))))))

(defn decrease [rec]
  (let [speed (:speed rec)]
  (update-in rec [:speed] (fn [n] (min 1.0 (+ n 0.25))))))

(defn turnr [rec]
  (update-in rec [:heading] (fn [n] (+ 0.1 n))))

(defn turnl [rec]
  (update-in rec [:heading] (fn [n] (- n 0.1))))

(defn key-pressed [state event]
  "Process key event.
   p will pause/unpause everything."
  (condp = (:key event)
    :p (update-in state [:running?] not)
    :up (update-in state [:rec] increase)
    :down (update-in state [:rec] decrease)
    :left (update-in state [:rec] turnl)
    :right (update-in state [:rec] turnr)
    :y (if (or (>= @num maxpoint) (= @timer 0))
       (update-in state [:circles] 
       (fn [n] (reset! num 0) (reset! timer init)
         (vec (make-many-circles (:n state) (:size state))))) state)
    :n (if (or (>= @num maxpoint) (= @timer 0)) (reset! redraw false) state)
    state))
    

(q/defsketch circles
    :host "host"
    :size [500 500]                ;we need a square canvas
    :setup setup                   ;getting things started, setting initial state
    :update update-state           ;the function to update the state
    :draw draw-state               ;the necessary draw function
    :mouse-clicked mouse-clicked   ;this is our mouse click event
    :key-pressed key-pressed       ;this is our keyboard input event
    :middleware [m/fun-mode])      ;this gives us the ability to have state
