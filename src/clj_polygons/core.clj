(ns clj-polygons.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [loom.alg :as alg]
            [loom.derived :as derived]
            [delaunay-triangulation.core :as delaunay]
            [clojure.set]
            [clojure.pprint :refer [pprint]]))

(defn make-points [step-size threshold width height]
  (set
    (for [x (range 0 width step-size)
          y (range 0 height step-size)
          :when (< (q/random 1) threshold)]
      {:x x :y y})))

(defn to-points-vec [points]
  (map (fn [point] [(:x point) (:y point)]) points))

(defn triangulate [points]
  (let [points-vec (to-points-vec points)
        triangulation (delaunay/triangulate points-vec)]
    triangulation))

(defn get-faces [{:keys [points edges triangles]}]
  (map
   (fn [[p1 p2 p3]]
     {:point1 {:x (first p1) :y (second p1)}
      :point2 {:x (first p2) :y (second p2)}
      :point3 {:x (first p3) :y (second p3)}})
   triangles))

;; Points are ordered clockwise from 12 o'clock.
(defn point-less-than? [point1 point2]
  (let [center-x (/ (q/width) 2)
        center-y (/ (q/height) 2)
        det (-
             (*
              (- (:x point1) center-x)
              (- (:y point2) center-y))
             (*
              (- (:x point2) center-x)
              (- (:y point1) center-y)))]
    (cond
      (< det 0)
      true
      (> det 0)
      false
      (= det 0)
      (let [d1 (+
                (*
                 (- (:x point1) center-x)
                 (- (:x point1) center-x))
                (*
                 (- (:y point1) center-y)
                 (- (:y point1) center-y)))
            d2 (+
                (*
                 (- (:x point2) center-x)
                 (- (:x point2) center-x))
                (*
                 (- (:y point2) center-y)
                 (- (:y point2) center-y)))]
        (> d1 d2)))))

(defn make-edge [point1 point2]
  (let [start (if (point-less-than? point1 point2) point1 point2)
        end (if (= start point1) point2 point1)]
    {:start start
     :end end}))

;; The outer boundary is the longest contiguous 
;; set of edges that only appear in a single face.
;; 
;; So first we find all edges that only appear in a single face.
;; Then we find the longest contiguous edge. We do this to remove
;; "inner hole" artifacts of the Delaunay triangulation.

(defn get-edges [{:keys [point1 point2 point3 :as face]}]
  [(make-edge point1 point2)
   (make-edge point2 point3)
   (make-edge point3 point1)])

(defn has-edge? [face edge]
  (let [edges (get-edges face)]
    (some #(= edge %) edges)))

(defn get-boundary-faces [faces boundary-edges]
  (filter
   (fn [face]
     (some #(has-edge? face %) boundary-edges))
   faces))

(defn get-boundary [faces]
  (let [all-edges (flatten
                   (map
                    get-edges
                    faces))
        edge-counts (frequencies all-edges)
        boundary-candidates (->> edge-counts
                                 (filter (fn [[edge count]] (= count 1)))
                                 (map #(first %)))
        boundary-graph (reduce
                        (fn [g edge]
                          (loom.graph/add-edges g [(:start edge) (:end edge)]))
                        (loom.graph/graph)
                        boundary-candidates)
        boundary-components (alg/connected-components boundary-graph)
        longest-boundary-component (reduce
                                    (fn [largest current]
                                      (if (> (count current) (count largest))
                                        current
                                        largest))
                                    boundary-components)
        as-graph (derived/subgraph-reachable-from boundary-graph (first longest-boundary-component))
        boundary-edges (loom.graph/edges as-graph)]
    (map
     (fn [[start end]] (make-edge start end))
     boundary-edges)))

(defn initial-state []
  (let [step-size 10
        threshold 0.05
        points (make-points step-size threshold (q/width) (q/height))]
    {:step-size step-size
     :points points}))

(defn setup []
  (q/no-loop)
  (q/frame-rate 30)
  (q/stroke 204 102 0)
  (q/fill 204 102 0)
  (q/color-mode :rgb)
  (initial-state))

(defn update-state [state]
  state)

#_(def comp (atom nil))
#_(def g (atom nil))

(defn shrink-polygon [faces rounds]
  (loop [round 0
         faces faces]
    (let [boundary-edges (get-boundary faces)
          boundary-faces (get-boundary-faces faces boundary-edges)]
      (println round "of" rounds)
      (if (< round rounds)
        (do
          (let [random-boundary-face (rand-nth boundary-faces)
                new-faces (clojure.set/difference (set faces) (set [random-boundary-face]))]
            (recur (+ 1 round)
                   new-faces)))
        faces))))

(defn draw-state [{:keys [points] :as state}]
  (q/background 250)
  (doseq [point points]
    (q/point (:x point) (:y point)))
  (let [triangulation (triangulate points)
        faces (get-faces triangulation)]
    (doseq [{:keys [point1 point2 point3]} faces]
      (let [r (q/random 255)
            g (q/random 255)
            b (q/random 255)]
        (q/stroke r g b)
        (q/fill r g b)
        (q/triangle
         (:x point1) (:y point1)
         (:x point2) (:y point2)
         (:x point3) (:y point3))))

    (let [boundary-edges (get-boundary faces)
          boundary-faces (get-boundary-faces faces boundary-edges)]
      (doseq [{:keys [start end]} boundary-edges]
        (q/stroke-weight 3)
        (q/stroke 0 0 0)
        (q/line (:x start) (:y start) (:x end) (:y end)))
      (doseq [{:keys [point1 point2 point3]} boundary-faces]
        (q/fill 0 0 0)
        (q/triangle
         (:x point1) (:y point1)
         (:x point2) (:y point2)
         (:x point3) (:y point3))
        )

      (let [p (shrink-polygon faces (/ (count faces) 2))]
        (doseq [{:keys [point1 point2 point3]} p]
          (q/fill 255 0 0)
          (q/stroke 255 0 0)
          (q/triangle
           (:x point1) (:y point1)
           (:x point2) (:y point2)
           (:x point3) (:y point3))
          )
        )
      )
    )
  )

(defn point-to-vec [point]
  [(:x point) (:y point)])

(q/defsketch clj-polygons
  :title "Polygons"
  :size [700 800]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
