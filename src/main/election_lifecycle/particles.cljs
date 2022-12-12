(ns election-lifecycle.particles
  (:require [quil.core :as q]
            [election-lifecycle.vector :as vector]
            [election-lifecycle.utils :as u]))

(comment
  ;; A particle looks like this:
  {:spawn-position [3 4]
   :velocity       [5 6]
   :spawn-time     1234
   :end-of-life    1234515
   :start-radius   69
   :size-update    :shrink ; can be either :shrink or :constant
   :color-config   {:constant [14 46 134 123]}
   ;; color-config can also be:
   #_{:interpolate {:start [14 46 134 123]
                    :end   [14 46 134 123]}}
   }
  )

(defonce particle-buffer (atom []))
(defonce emitters (atom []))

(def x first)
(def y second)

(defn clear! []
  (reset! particle-buffer []))

(defn radius [{:keys [start-radius spawn-time end-of-life size-update] :as _particle} current-time-ms]
  (case size-update
    :constant start-radius
    :shrink (if (>= current-time-ms end-of-life)
              0
              (q/map-range (- end-of-life current-time-ms)
                           0
                           (- end-of-life spawn-time)
                           0
                           start-radius))))

(defn position [{:keys [spawn-position spawn-time velocity]} current-time-ms]
  (if (<= current-time-ms spawn-time)
    spawn-position
    (vector/add spawn-position (vector/scale velocity
                                             (/ (- current-time-ms spawn-time) 1000) ; convert ms to seconds because the velocity is in units/second
                                             ))))

(defn color [{:keys [color-config spawn-time end-of-life] :as _particle} current-time-ms]
  (cond
    (:constant color-config) (:constant color-config)
    (:interpolate color-config) (let [color-obj (q/lerp-color (apply q/color (get-in color-config [:interpolate :start]))
                                                              (apply q/color (get-in color-config [:interpolate :end]))
                                                              (q/map-range current-time-ms
                                                                           spawn-time
                                                                           end-of-life
                                                                           0
                                                                           1))]
                                  [(q/red color-obj)
                                   (q/green color-obj)
                                   (q/blue color-obj)
                                   (q/alpha color-obj)])))

(defn dead? [current-time {:keys [end-of-life] :as _particle}]
  (<= end-of-life current-time))

(defn add-particle!
  [current-time spawn-position velocity start-radius lifespan size-update color]
  (swap! particle-buffer conj {:spawn-position spawn-position
                               :velocity       velocity
                               :spawn-time     current-time
                               :end-of-life    (+ current-time lifespan)
                               :start-radius   start-radius
                               :size-update    size-update
                               :color-config   color}))

(defn garbage-collect-particles! [current-time]
  (swap! particle-buffer (comp vec (partial remove
                                            (partial dead? current-time)))))

(defn spray-particles!
  [spawn
   volume
   radius
   size-update
   color-config
   velocity
   lifespan
   spawn-jitter-factors
   velocity-jitter-factors
   lifespan-jitter]
  (dotimes [_ volume]
    (add-particle! (q/millis)
                   (u/gaussian-jitter-vector spawn spawn-jitter-factors)
                   (u/gaussian-jitter-vector velocity velocity-jitter-factors)
                   radius
                   (u/gaussian-jitter lifespan lifespan-jitter)
                   size-update
                   color-config)))

(defn clear-emitters! []
  (doseq [interval @emitters]
    (js/clearInterval interval))
  (reset! emitters []))

(defn add-emitter!
  [interval
   volume
   radius
   size-update
   color-config
   spawn
   velocity
   lifespan
   spawn-jitter-factors
   velocity-jitter-factors
   lifespan-jitter]
  (swap!
    emitters
    conj
    (js/setInterval
      (fn []
        (q/with-sketch (q/get-sketch-by-id "canvas-id")
          (dotimes [_ volume]
            (add-particle! (q/millis)
                           (u/gaussian-jitter-vector spawn spawn-jitter-factors)
                           (u/gaussian-jitter-vector velocity velocity-jitter-factors)
                           radius
                           (u/gaussian-jitter lifespan lifespan-jitter)
                           size-update
                           color-config))))
      interval)))
