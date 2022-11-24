(ns election-lifecycle.sketch
  (:require [quil.core :as q]
            [election-lifecycle.vertex-buffer :as vb]
            [election-lifecycle.line :as line]
            [election-lifecycle.utils :as u]))

(def canvas-width 1200)
(def canvas-height 900)
(def canvas-centre [(/ canvas-width 2) (/ canvas-height 2)])

(defn- generate-tentacle-vertices
  [start-vertex]
  (let [tentacle-length (+ 100 (rand-int 100))
        divided-line    (-> (line/line-given-length start-vertex canvas-centre tentacle-length)
                            (line/divide-line 20))]
    (doall (cons (first divided-line)
                 (line/distort-segments (rest divided-line)
                                        70)))))

(defn- generate-tentacle
  [start-vertex]
  {:type      :line-list
   :vertices  (generate-tentacle-vertices start-vertex)
   :animation nil
   :meta      {:category     :tentacle
               :start-vertex start-vertex}})

(defn- animate-tentacle
  [tentacle-shape current-time animation-time]
  (assoc tentacle-shape
    :animation {:target-vertices (generate-tentacle-vertices (get-in tentacle-shape [:meta :start-vertex]))
                :start-time      current-time
                :end-time        (+ animation-time current-time)}))

(defn- init-tentacles
  []
  (let [top-origins    (map #(vector % 0) (range 150 canvas-width 300))
        bottom-origins (map (fn [[x y]]
                              [x (+ canvas-height y)])
                            top-origins)
        left-origins   (map #(vector 0 %) (range 150 canvas-height 300))
        right-origins  (map (fn [[x y]]
                              [(+ canvas-width x) y])
                            left-origins)]
    (doseq [origin (concat top-origins bottom-origins left-origins right-origins)]
      (vb/add-shape! (generate-tentacle origin)))))


(defonce red-overlay (atom nil))

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
  (let [max-distance    (u/distance [0 0] canvas-centre)
        display-density (q/display-density img)
        pixels          (q/pixels img)]
    (dotimes [x canvas-width]
      (dotimes [y canvas-height]
        (set-pixel pixels
                   display-density
                   canvas-width
                   x
                   y
                   (q/color 255 0 0 (* 255 (/ (- (u/distance [x y] canvas-centre) 300)
                                              max-distance))))))
    (q/update-pixels img)
    img))

(defn- create-overlay []
  (let [overlay (q/create-image canvas-width canvas-height)]
    (draw-gradient overlay)
    overlay))

(defn- blend-overlay []
  (q/blend @red-overlay
           0
           0
           canvas-width
           canvas-height
           0
           0
           canvas-width
           canvas-height
           :add))

(defn setup []
  (q/frame-rate 60)
  (q/background 0 0 0)
  (q/stroke 255 255 255)
  (vb/clear!)
  (init-tentacles)
  (reset! red-overlay (create-overlay))
  (blend-overlay))

(defn draw []
  (q/background 0 0 0)
  (blend-overlay)
  (swap! vb/vertex-buffer
         vb/update-shapes
         #(and (= :tentacle (get-in % [:meta :category]))
               (not (:animation %)))
         (fn [tentacle]
           (animate-tentacle tentacle (q/millis) 100))))
