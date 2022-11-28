(ns election-lifecycle.sketch
  (:require [quil.core :as q]
            [election-lifecycle.vertex-buffer :as vb]
            [election-lifecycle.line :as line]
            [election-lifecycle.utils :as u]
            [election-lifecycle.polygon :as polygon]))

(def canvas-width 1200)
(def canvas-height 900)
(def canvas-centre [0 0])
(def canvas-top-left [(- (/ canvas-width 2)) (- (/ canvas-height 2))])

(def x first)
(def y second)

(defn- generate-tentacle-vertices
  [start-vertex end-vertex tentacle-length]
  (let [divided-line (-> (line/line-given-length start-vertex end-vertex tentacle-length)
                         (line/divide-line 20))]
    (doall (cons (first divided-line)
                 (line/distort-segments (rest divided-line)
                                        70)))))

(defn- generate-tentacle
  [start-vertex end-vertex tentacle-length]
  {:type      :line-list
   :vertices  (generate-tentacle-vertices start-vertex end-vertex tentacle-length)
   :animation nil
   :stroke    [96 91 102 125]
   :fill      [0 0 0 0]
   :meta      {:category     :tentacle
               :start-vertex start-vertex}})

(defn- tentacle-length
  [start-position end-position]
  (min (+ 100 (rand-int 100))
       (- (u/distance start-position end-position) 50)))

(defn- animate-tentacle
  [tentacle-shape current-time animation-time end-vertex]
  (assoc tentacle-shape
    :animation {:target-vertices (generate-tentacle-vertices (get-in tentacle-shape [:meta :start-vertex])
                                                             end-vertex
                                                             (tentacle-length (get-in tentacle-shape [:meta :start-vertex])
                                                                              end-vertex))
                :start-time      current-time
                :end-time        (+ animation-time current-time)}))

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
      (vb/add-shape! (generate-tentacle origin canvas-centre tentacle-length)))))

(defn- screen-to-world
  "Converts on-screen pixel coordinates to the 3D coordinate system."
  [vertex]
  [(- (x vertex) (/ canvas-width 2))
   (- (y vertex) (/ canvas-height 2))])

(defn- set-pixel
  "Sets a color in a pixel array. Taken from https://p5js.org/reference/#/p5/pixels"
  [pixel-array display-density width x y color]
  (dotimes [i display-density]
    (dotimes [j display-density]
      (let [index (* 4 (+ (* (+ (* y display-density)
                                j)
                             width
                             display-density)
                          (+ (* x display-density)
                             i)))]
        (aset pixel-array index (q/red color))
        (aset pixel-array (+ 1 index) (q/green color))
        (aset pixel-array (+ 2 index) (q/blue color))
        (aset pixel-array (+ 3 index) (q/alpha color))))))

(defn- draw-gradient [img]
  (let [max-distance    (u/distance canvas-top-left [0 0])
        display-density (q/display-density img)
        pixels          (q/pixels img)]
    (dotimes [x canvas-width]
      (dotimes [y canvas-height]
        (set-pixel pixels
                   display-density
                   canvas-width
                   x
                   y
                   (q/color 51 1 105 (* 255 (/ (- (u/distance [x y] [(/ canvas-width 2) (/ canvas-height 2)]) 300)
                                               max-distance))))))
    (q/update-pixels img)
    img))

(defonce creepy-gradient (atom nil))

(defn- init-creepy-gradient []
  (let [overlay (q/create-image canvas-width canvas-height)]
    (draw-gradient overlay)
    (reset! creepy-gradient overlay)))

(defn- blend-gradient []
  (q/blend-mode :blend)
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

(defonce tie-texture (atom nil))

(defn- tie-bottom [centre-vertex]
  (let [start-vertex [(- (x centre-vertex) 10) (- (y centre-vertex) 25)]]
    (-> (polygon/make-polygon
          [start-vertex
           [(+ 20 (x start-vertex)) (y start-vertex)]
           [(+ 25 (x start-vertex)) (+ 80 (y start-vertex))]
           [(+ 10 (x start-vertex)) (+ 90 (y start-vertex))]
           [(- (x start-vertex) 5) (+ 80 (y start-vertex))]])
        (assoc-in [:meta :category] :tie-bottom)
        (assoc :stroke [0 0 255 255])
        (assoc :fill [0 0 255 255]))))

(defn- init-tie-texture []
  (let [gr (q/create-graphics 100 100)]
    (q/with-graphics gr
      (q/background 0 0 255 255)
      (q/fill 255)
      (q/with-rotation [(/ q/PI 6)]
        (doseq [y (range -100 100 10)]
          (q/rect 0 y 200 3))))
    (reset! tie-texture gr)))

(defn setup []
  (q/frame-rate 60)
  (q/ortho)
  (q/background 0 0 0)
  (q/stroke 255 255 255)
  (vb/clear!)
  (init-tentacles)
  (init-tie-texture)
  (init-creepy-gradient)
  (blend-gradient)
  (vb/add-shape! (tie-bottom canvas-centre)))

(defn- mouse-position []
  (screen-to-world [(q/mouse-x) (q/mouse-y)]))

(defn draw []
  (q/background 0 0 0)
  (blend-gradient)
  (let [the-mouse-position (mouse-position)]
    (swap! vb/vertex-buffer
           vb/update-shapes
           #(and (= :tentacle (get-in % [:meta :category]))
                 (not (:animation %)))
           (fn [tentacle]
             (animate-tentacle tentacle
                               (q/millis)
                               100
                               (tentacle-end-position the-mouse-position))))))
