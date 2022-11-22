(ns election-lifecycle.core
  (:require [reagent.dom :as rdom]
            [quil.core :as q]
            [election-lifecycle.line :as line]
            [election-lifecycle.drawing :as drawing]
            [clojure.walk :as walk])
  (:require-macros [quil.core]))

(defn some-component []
  [:div
   [:h3 "Election Reddy's tragic past"]])

(def canvas-width 1200)
(def canvas-height 900)
(def canvas-centre [(/ canvas-width 2) (/ canvas-height 2)])

(def tentacles-last-changed (atom 0))

(comment
  ;; The vertex buffer contains a list of shapes which are to be rendered.
  ;; Shapes might look something like this:

  {:type      :line-list
   :vertices  []
   :animation {:target-vertices []
               :start-time      50
               :end-time        100}
   :meta      {:category :tentacle}}

  ;; One can add fields for stroke colour, fill colour and so on as well.

  ;; An animator function will compute the final positions of each shape and render them.
  ;; A second function will garbage-collect finished animations.
  )

(def vertex-buffer (atom []))

(defn add-shape! [shape]
  (swap! vertex-buffer conj shape))

(defn- generate-tentacle-vertices
  [start-vertex]
  (let [tentacle-length (+ 100 (rand-int 100))]
    (-> (line/line-given-length start-vertex canvas-centre tentacle-length)
        (line/divide-line 20)
        (line/distort-segments 70)
        doall)))

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
      (add-shape! (generate-tentacle origin)))))

(defn setup []
  (q/frame-rate 60)
  (q/background 255 0 0)
  (q/stroke 255 255 255)
  (init-tentacles))

(defn update-shapes [shapes filter-fn update-fn]
  (walk/postwalk
    (fn [shape]
      (if (and (:type shape)
               (:vertices shape)
               (filter-fn shape))
        (update-fn shape)
        shape))
    shapes))

(defn update-by-category [shapes category update-fn]
  (update-shapes shapes
                 #(= category (get-in % [:meta :category]))
                 update-fn))

(defn cleanup-finished-animations! [current-time]
  (swap! vertex-buffer
         update-shapes
         :animation
         (fn [{:keys [animation] :as shape}]
           (if (<= (:end-time animation) current-time)
             (assoc shape
               :vertices (:target-vertices animation)
               :animation nil)
             shape))))

(defn draw []
  (q/background 255 0 0)
  (q/fill 255 255 255)
  (q/text (Math/round (q/current-frame-rate)) 10 20)
  (doseq [shape @vertex-buffer]
    (drawing/draw-shape shape))
  (cleanup-finished-animations! (q/millis))


  (swap! vertex-buffer
         update-shapes
         #(and (= :tentacle (get-in % [:meta :category]))
               (not (:animation %)))
         (fn [tentacle]
           (animate-tentacle tentacle (q/millis) 500))))

(q/defsketch example
  :host "canvas-id"
  :setup setup
  :draw draw
  :size [canvas-width canvas-height])

(defn init []
  (rdom/render [some-component]
               (.getElementById js/document "app")))
