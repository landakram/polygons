(ns clj-polygons.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [loom.alg :as alg]
            [loom.derived :as derived]
            [delaunay-triangulation.core :as delaunay]
            [clojure.set]
            [clojure.pprint :refer [pprint]]))

(defn make-points [{:keys [threshold grid]}]
  (let [{:keys [width height step-size origin]} grid]
    (set
     (for [x (range (:x origin) (+ (:x origin) width) step-size)
           y (range (:y origin) (+ (:y origin) height) step-size)
           :when (< (q/random 1) threshold)]
       {:x x :y y}))))

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

(defn make-polygon [{:keys [threshold grid] :as params}]
  (let [points (make-points params)
        triangulation (triangulate points)
        faces (get-faces triangulation)]
    {:faces faces
     :round 0
     :rounds (/ (count faces) 2)}))

(defn scale [{:keys [width height] :as grid} percent]
  (merge grid {:width (* width percent) :height (* height percent)}))

(defn translate [{ :keys [origin] :as grid} dx dy]
  (let [x (:x origin)
        y (:y origin)]
    (merge grid {:origin {:x (+ x dx) :y (+ y dy)}})))

(defn initial-state []
  (let [grid {:width (q/width)
              :height (q/height)
              :origin {:x 0 :y 0}
              :step-size 10}
        polygon (make-polygon {:threshold 0.05
                               :grid (-> grid
                                         (scale 0.75)
                                         (translate 100 100))})]
    {:grid grid
     :noise {:counter 0
             :step 0.01}
     :polygon polygon}))

(defn setup []
  (q/frame-rate 30)
  #_(q/smooth 4)
  (q/stroke 204 102 0)
  (q/fill 204 102 0)
  (q/color-mode :rgb)
  (initial-state))

#_(quil.applet/with-applet clj-polygons
  (make-polygon {:threshold 0.5 :grid {:width 500 :height 500 :step-size 10}}))

(defn find-center [points]
  (let [xs (map #(:x %) points)
        ys (map #(:y %) points)
        x-center (/ (reduce + xs) (count xs))
        y-center (/ (reduce + ys) (count ys))]
    {:x x-center
     :y y-center}))


(defn pow2 [x] (Math/pow x 2))
(defn point-distance-from [p1 p2]
 (Math/sqrt (+ (pow2 (Math/abs (- (:x p1) (:x p2))))
                (pow2 (Math/abs (- (:y p1) (:y p2)))))))

(defn face-distance-from [{:keys [point1 point2 point3]} point]
  (+
   (point-distance-from point1 point)
   (point-distance-from point2 point)
   (point-distance-from point3 point)))


(defn sort-by-distance-from-center [boundary-faces]
  (let [points (flatten (map vals boundary-faces))
        center (find-center points)
        sorted-by-closest (sort
                           (fn [left right]
                             (< 
                              (face-distance-from left center)
                              (face-distance-from right center)))
                           boundary-faces)]
    sorted-by-closest))

(defn select-boundary-face [boundary-faces noise]
  ;; Add some heurstics here:
  ;;
  ;; We want to prefer boundary-faces that are closer to the center of the polygon as a whole
  ;;
  ;; Later, we'll want to prefer boundary-faces that are not abutting another polygon
  (let [take-threshold (* (count boundary-faces) 0.5)]
    (println take-threshold)
    (if (> (q/random 1) 0.3)
      (do 
        (println "random")
        (rand-nth boundary-faces))
      (do
        (println "closest")
        (rand-nth (take take-threshold (sort-by-distance-from-center boundary-faces))))
      ))
  #_(rand-nth boundary-faces)
  #_(let [noise-val (q/noise (:counter noise))
        the-nth (Math/round (* (- (count boundary-faces) 1) noise-val))]
    (println noise-val)
    (println the-nth)
    (nth boundary-faces the-nth)))

(defn shrink-polygon [polygon rounds noise]
  (loop [round 0
         faces (:faces polygon)]
    (let [boundary-edges (get-boundary faces)
          boundary-faces (get-boundary-faces faces boundary-edges)]
      (println round "of" rounds)
      (if (< round rounds)
        (do
          (let [random-boundary-face (select-boundary-face boundary-faces noise)
                new-faces (clojure.set/difference (set faces) (set [random-boundary-face]))]
            (recur (+ 1 round)
                   new-faces)))
        (merge polygon
               {:faces faces
                :round (+ (:round polygon) rounds)})))))

(defn update-polygon [polygon noise]
  (let [{:keys [round rounds]} polygon
        new-round (+ 2 (:round polygon round))]
    (if (>= new-round rounds)
      polygon
      (shrink-polygon polygon 2 noise))))

(defn update-state [{:keys [polygon noise] :as state}]
  (merge
   state
   {:polygon (update-polygon polygon noise)
    :noise {:counter (+ (:step noise) (:counter noise))
            :step (:step noise)}}))

(defn draw-state [{:keys [polygon] :as state}]
  (let [bg-col {:r 31 :g 31 :b 30}]
    (q/background (:r bg-col) (:g bg-col) (:b bg-col))
    (doseq [{:keys [point1 point2 point3]} (:faces polygon)]
      (q/no-fill)
      (q/stroke 255 255 255)
      (q/triangle
       (:x point1) (:y point1)
       (:x point2) (:y point2)
       (:x point3) (:y point3))
      ))
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

