(ns election-lifecycle.sketch
  (:require [quil.core :as q]
            [election-lifecycle.vertex-buffer :as vb]
            [election-lifecycle.line :as line]
            [election-lifecycle.utils :as u]
            [election-lifecycle.polygon :as polygon]
            [election-lifecycle.particles :as particles]
            [election-lifecycle.vector :as vector]
            [election-lifecycle.tentacle :as tentacle]))

(def canvas-width 1200)
(def canvas-height 900)
(def canvas-centre [0 0])
(def canvas-top-left [(- (/ canvas-width 2)) (- (/ canvas-height 2))])

(def x first)
(def y second)

(defonce creepy-gradient (atom nil))
(defonce tie-texture (atom nil))

(defn- init-tentacles
  []
  (let [top-origins     (map #(vector % (- (/ canvas-height 2)))
                             (range (+ (- (/ canvas-width 2))
                                       150)
                                    (/ canvas-width 2)
                                    300))
        bottom-origins  (map (fn [[x y]]
                               [x (+ canvas-height y)])
                             top-origins)
        left-origins    (map #(vector (- (/ canvas-width 2)) %)
                             (range -300 (/ canvas-height 2) 150))
        right-origins   (map (fn [[x y]]
                               [(+ canvas-width x) y])
                             left-origins)
        tentacle-length (rand-int 100)]
    (doseq [origin (concat top-origins bottom-origins left-origins right-origins)]
      (vb/add-shape! (tentacle/generate-tentacle origin canvas-centre tentacle-length)))))

(defn- draw-gradient [img]
  (let [max-distance (u/distance canvas-top-left [0 0])]
    (dotimes [x canvas-width]
      (dotimes [y canvas-height]
        (q/set-pixel img x y (q/color
                               255 0 10
                               (* 255 (/ (- (u/distance [x y] [(/ canvas-width 2) (/ canvas-height 2)])
                                            300)
                                         max-distance))))))
    (q/update-pixels img)
    img))

(defn- init-creepy-gradient []
  (let [overlay (q/create-image canvas-width canvas-height)]
    (draw-gradient overlay)
    (reset! creepy-gradient overlay)))

(defn- blend-gradient []
  (q/image @creepy-gradient -600 -450))

(defn- out-of-bounds?
  [vertex]
  (not (and (<= (- (/ canvas-width 2)) (x vertex) (/ canvas-width 2))
            (<= (- (/ canvas-height 2)) (y vertex) (/ canvas-height 2)))))

(defn- tentacle-end-position
  [mouse-position]
  (if (out-of-bounds? mouse-position)
    canvas-centre
    mouse-position))

(defn- tie-top [centre-vertex]
  (let [start-vertex [(- (x centre-vertex) 20) (- (y centre-vertex) 50)]]
    (-> (polygon/make-polygon
          [start-vertex
           [(+ 40 (x start-vertex)) (y start-vertex)]
           [(+ 50 (x start-vertex)) (- (y start-vertex) 20)]
           [(- (x start-vertex) 10) (- (y start-vertex) 20)]])
        (assoc-in [:meta :category] :tie-top)
        (assoc :texture @tie-texture))))

(defn- tie-bottom [centre-vertex]
  (let [start-vertex [(- (x centre-vertex) 20) (- (y centre-vertex) 50)]]
    (-> (polygon/make-polygon
          [start-vertex
           [(+ 40 (x start-vertex)) (y start-vertex)]
           [(+ 50 (x start-vertex)) (+ 160 (y start-vertex))]
           [(+ 20 (x start-vertex)) (+ 180 (y start-vertex))]
           [(- (x start-vertex) 10) (+ 160 (y start-vertex))]])
        (assoc-in [:meta :category] :tie-bottom)
        (assoc :texture @tie-texture))))

(defn- init-tie-texture []
  (let [gr (q/create-graphics canvas-width canvas-height)]
    (q/with-graphics gr
      (q/background 0 0 255 255)
      (q/fill 255)
      (q/with-rotation [(/ q/PI 6)]
        (doseq [y (range -900 900 10)]
          (q/rect 0 y 1500 3))))
    (reset! tie-texture gr)))

(defn- spawn-fire [spawn size]
  ;; flames
  (particles/add-emitter! 10
                          (* 5 size)
                          (* 4 size)
                          :shrink
                          {:interpolate {:start [252 (* 7 size) 3 25]
                                         :end   [252 0 3 0]}}
                          spawn
                          [0 (* -20 size)]
                          (* size 200)
                          [size (/ size 2.5)]
                          [(* 3 size) (* 2 size)]
                          (* size 40))
  ;; smoke
  (particles/add-emitter! 10
                          (* 5 size)
                          (* 4 size)
                          :shrink
                          {:interpolate {:start [30 30 30 25]
                                         :end   [10 10 10 175]}}
                          spawn
                          [0 (* -20 size)]
                          (* size 400)
                          [size (/ size 2.5)]
                          [(* 4 size) (* 2 size)]
                          (* size 40)))

(defn setup []
  (q/background 0 0 0)
  (q/stroke 255 255 255)
  (vb/clear!)
  (init-tentacles)
  (init-tie-texture)
  (init-creepy-gradient)
  (blend-gradient)
  (vb/add-shape! (tie-bottom canvas-centre))
  (vb/add-shape! (tie-top canvas-centre))
  (spawn-fire (vector/add canvas-centre [4 110]) 4)
  (spawn-fire (vector/add canvas-centre [3.5 -25]) 3.2)
  (spawn-fire (vector/add canvas-centre [2 25]) 1.5)
  (spawn-fire (vector/add canvas-centre [-10 50]) 2))

(defn- mouse-position []
  (u/screen-to-world [(q/mouse-x) (q/mouse-y)] canvas-width canvas-height))

(defn draw []
  (q/background 0 0 0)
  (blend-gradient)
  (let [the-mouse-position (mouse-position)]
    (swap! vb/vertex-buffer
           vb/update-shapes
           #(and (= :tentacle (get-in % [:meta :category]))
                 (not (:animation %)))
           (fn [tentacle]
             (tentacle/animate-tentacle tentacle
                                        (q/millis)
                                        300
                                        (tentacle-end-position the-mouse-position))))))