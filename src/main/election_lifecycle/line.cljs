(ns election-lifecycle.line
  (:require [quil.core :as q]))

(def x first)
(def y second)

(defn perlin-noise-seq
  "Generate a lazy infinite sequence of perlin noise values starting from
  the specified seed with incr added to the seed for each successive value.
  Ranges from -0.5 to +0.5."
  [seed incr]
  (lazy-seq (cons (- (q/noise seed) 0.5) (perlin-noise-seq (+ seed incr) incr))))

(defn- slope [p1 p2]
  (/ (- (y p2) (y p1))
     (- (x p2) (x p1))))

(defn- zip-coords [x-coords y-coords]
  (map vector x-coords y-coords))

(defn- divide-line [[p1 p2] n-segments]
  (let [x-coord-step (/ (- (x p2) (x p1))
                        n-segments)
        x-coords     (range (x p1)
                            (+ (x p2) 0.1)
                            x-coord-step)
        y-intercept  (- (y p1) (* (slope p1 p2) (x p1)))
        y-coords     (map (fn [x] (+ (* (slope p1 p2) x)
                                     y-intercept))
                          x-coords)]
    (zip-coords x-coords y-coords)))

(defn- add-points [p1 p2]
  [(+ (x p1) (x p2))
   (+ (y p1) (y p2))])

(defn line-length [p1 p2]
  (Math/sqrt (+ (-> (- (x p2) (x p1))
                    (Math/pow 2))
                (-> (- (y p2) (y p1))
                    (Math/pow 2)))))

(defn line-given-length
  "Returns p1 and p3 such that p3 is on the same line as p1 and p2,
  but at a distance of given-length from p1.
  Taken from https://math.stackexchange.com/a/1630886"
  [p1 p2 given-length]
  (let [distance-ratio (/ given-length (line-length p1 p2))]
    [p1
     [(+ (* (- 1 distance-ratio)
            (x p1))
         (* distance-ratio (x p2)))
      (+ (* (- 1 distance-ratio)
            (y p1))
         (* distance-ratio (y p2)))]]))

(defn distort-segments
  "Moves the segment vertices around using Perlin noise.
  More deviance = more movement."
  [line-points deviance]
  (let [x-seed        (rand 10)
        y-seed        (rand 10)
        perlin-incr   0.1
        points-to-add (zip-coords (map (partial * deviance)
                                       (perlin-noise-seq x-seed perlin-incr))
                                  (map (partial * deviance)
                                       (perlin-noise-seq y-seed perlin-incr)))]
    (map add-points
         line-points
         points-to-add)))

