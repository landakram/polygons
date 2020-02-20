(ns clj-polygons.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [loom.alg :as alg]
            [loom.alg-generic :as alg-generic]
            [loom.graph :as graph]
            [loom.derived :as derived]
            [delaunay-triangulation.core :as delaunay]
            [clojure.set]
            [geo.poly :as poly]
            [clojure.pprint :refer [pprint]]))

(defn enumerate-points [grid]
  (let [{:keys [width height step-width step-height origin]} grid]
    (for [x (range (:x origin) (+ (:x origin) width) step-width)
          y (range (:y origin) (+ (:y origin) height) step-height)]
      {:x x :y y})))


(defn create-tiles [grid sections]
  (let [tile-width (/ (:width grid) sections)
        tile-height (/ (:height grid) sections)]
    (map
     (fn [point]
       (merge grid {:origin point :width tile-width :height tile-height}))
     (enumerate-points (merge grid {:step-width tile-width :step-height tile-height})))))


(defn randomly-subdivide [tiles]
  (flatten
   (map 
    (fn [tile]
      (if (< (rand 1) 0.5)
        (create-tiles tile 2)
        [tile]))
    tiles)))

(defn randomly-subdivided-tiles [grid rounds]
  (let [tiles (create-tiles grid 1)]
    (nth (iterate randomly-subdivide tiles) rounds)))

(defn make-points [{:keys [threshold grid]}]
  (set
   (filter
    (fn [p] (< (q/random 1) threshold))
    (enumerate-points grid))))

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
(defn point-less-than? [point1 point2 center-x center-y]
  (let [det (-
             (*
              (- (:x point1) center-x)
              (- (:y point2) center-y))
             (*
              (- (:x point2) center-x)
              (- (:y point1) center-y)))]
    (cond
      (< (float det) 0.0)
      (do
        true)
      (> (float det) 0.0)
      (do
        false)
      (= (float det) 0.0)
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

(defn center-x []
  (/ (q/width) 2))

(defn center-y []
  (/ (q/height) 2))

(defn center-point []
  {:x (center-x) :y (center-y)})

(defn make-edge [point1 point2]
  (let [start (if (point-less-than? point1 point2 (:x (center-point)) (:y (center-point))) point1 point2)
        end (if (= start point1) point2 point1)]
    {:start start
     :end end}))


(defn get-edges [{:keys [point1 point2 point3 :as face]}]
  [(make-edge point1 point2)
   (make-edge point2 point3)
   (make-edge point3 point1)])

(defn edges-equal? [edge1 edge2]
  (or
   (and
    (= (:start edge1) (:start edge2))
    (= (:end edge1) (:end edge2)))
   (and
    (= (:start edge1) (:end edge2))
    (= (:end edge1)) (:start edge2))))

(defn has-edge? [face edge]
  (let [edges (get-edges face)]
    (some
     (fn [e]
       (edges-equal? e edge))
     edges)))

(defn get-points [faces]
  (flatten (map vals faces)))

(defn find-center [points]
  (let [xs (map #(:x %) points)
        ys (map #(:y %) points)
        x-center (/ (reduce + xs) (count xs))
        y-center (/ (reduce + ys) (count ys))]
    {:x x-center
     :y y-center}))

(defn get-boundary-faces [faces boundary-edges]
  (filter
    (fn [face]
      (some #(has-edge? face %) boundary-edges))
    faces))

(defn random-grayscale []
  (let [r (q/random 255)
        g r
        b r]
    {:r r :g g :b b}))

(defn make-color [r g b]
  {:r r :g g :b b})

(defn random-from-theme []
  (let [colors [(make-color 255 174 143)
                (make-color 255 103 125)
                (make-color 205 102 132)
                (make-color 111 90 126)]]
    (rand-nth colors)))

(defn face-colors [faces]
  (into
   {}
   (map
    (fn [face]
      [face (random-grayscale)])
    faces)))

(defn make-polygon [{:keys [threshold grid] :as params}]
  (let [points (make-points params)
        triangulation (triangulate points)
        faces (get-faces triangulation)]
    {:faces faces
     :round 1
     :face-colors (face-colors faces)
     :rounds (/ (count faces) 2)}))

(defn scale [{:keys [width height] :as grid} percent]
  (merge grid {:width (* width percent) :height (* height percent)}))

(defn translate [{ :keys [origin] :as grid} dx dy]
  (let [x (:x origin)
        y (:y origin)]
    (merge grid {:origin {:x (+ x dx) :y (+ y dy)}})))

(defn tiled-polygons [grid rounds]
  (let [tiles (randomly-subdivided-tiles grid rounds)]
    (map
     (fn [tile]
       (let [scale-factor (/ (:width tile) (:width grid))
             threshold (if (< scale-factor 0.25) 0.2 0.1)]
         (make-polygon {:threshold threshold :grid tile})))
     tiles)))

(defn random-polygons [n grid]
  (let [threshold 0.05]
    (map
     (fn [i]
       (let [scale-val (q/random 0.4 0.7)
             origin-x (q/random 0 (- (q/width) 200))
             origin-y (q/random 0 (- (q/height) 200))]
         (make-polygon {:threshold threshold
                        :grid (-> grid
                                  (scale scale-val)
                                  (translate origin-x origin-y))})))
     (range n))))

(defn four-polygons [grid]
  [(make-polygon {:threshold 0.05
                  :grid (-> grid
                            (scale 0.5)
                            (translate 0 0))})
   (make-polygon {:threshold 0.05
                  :grid (-> grid
                            (scale 0.5)
                            (translate (/ (q/width) 2) 0))})
   (make-polygon {:threshold 0.05
                  :grid (-> grid
                            (scale 0.5)
                            (translate 0 (/ (q/height) 2)))})
   (make-polygon {:threshold 0.05
                  :grid (-> grid
                            (scale 0.5)
                            (translate (/ (q/width) 2) (/ (q/height) 2)))})])

(defn initial-state []
  (let [grid {:width (q/width)
              :height (q/height)
              :origin {:x 0 :y 0}
              :step-width 10
              :step-height 10}]
    {:grid grid
     :polygons
     (tiled-polygons grid 3)
     #_(four-polygons grid)
     #_[{:faces [{:point1 {:x 100 :y 100}
                :point2 {:x 300 :y 100}
                :point3 {:x 200 :y 200}}

               {:point1 {:x 300 :y 100}
                :point2 {:x 200 :y 200}
                :point3 {:x 200 :y 400}}
               ]
       :round 0
       :face-colors :hi
       :rounds 1}
      ]
     #_[(first (four-polygons grid))]
     #_(random-polygons 6 grid)}))

(defn setup []
  (q/frame-rate 30)
  #_(q/smooth 4)
  (q/stroke 204 102 0)
  (q/fill 204 102 0)
  (q/color-mode :rgb)
  (initial-state))

;; The outer boundary is the longest contiguous 
;; set of edges that only appear in a single face.
;; 
;; So first we find all edges that only appear in a single face.
;; Then we find the longest contiguous edge. We do this to remove
;; "inner hole" artifacts of the Delaunay triangulation.

(defn connect-edges [edges]
  )

#_(defn get-polygon-boundary [{:keys [faces]}]
  (let [points (get-points faces)
        center-point (find-center points)
        all-edges (flatten
                   (map
                    #(get-edges % center-point)
                    faces))
        edge-counts (frequencies all-edges)
        boundary-candidates (->> edge-counts
                                 (filter (fn [[edge count]] (= count 1)))
                                 (map #(first %)))
        largest-connected-edge 
        ]
    ))

(def x (atom nil))
(def f (atom nil))
(def ae (atom nil))
(def ec (atom nil))
(def bc (atom nil))
(def bg (atom nil))
(def g (atom nil))
(def s (atom nil))
(def e (atom nil))

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
        order-edges (fn [edges]
                      (loop [ordered-edges [(first edges)]
                             edges (rest edges)]
                        (let [last (last ordered-edges)
                              next (->> edges
                                        (filter (fn [e]
                                                  (or
                                                   (= (graph/src e) (graph/src last))
                                                   (= (graph/dest e) (graph/src last))
                                                   (= (graph/src e) (graph/dest last))
                                                   (= (graph/dest e) (graph/dest last)))))
                                        (first))
                              correctly-ordered (if (and (some? next)
                                                         (= (graph/dest next) (graph/dest last)))
                                                  [(graph/dest next) (graph/src next)]
                                                  next)]
                          (if (some? correctly-ordered)
                            (recur
                             (conj ordered-edges correctly-ordered)
                             (remove #(= next %) edges))
                            ordered-edges))))
        remove-duplicates (fn [edges]
                            (loop [deduped [(first edges)]
                                   edges (rest edges)]
                              (let [next (first edges)]
                                (if (some? next)
                                  (if (some #(or (and (= (graph/src next)
                                                         (graph/src %))
                                                      (= (graph/dest next)
                                                         (graph/dest %)))
                                                 (and (= (graph/src next)
                                                         (graph/dest %))
                                                      (= (graph/dest next)
                                                         (graph/src %))))
                                            deduped)
                                    (recur deduped
                                           (rest edges))
                                    (recur (conj deduped next)
                                           (rest edges)))
                                  deduped))))
        boundary-edges (-> (loom.graph/edges as-graph)
                           remove-duplicates
                           #_order-edges)]
    (map
     (fn [[start end]] (make-edge start end))
     boundary-edges)))

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
  (let [points (get-points boundary-faces)
        center (find-center points)
        sorted-by-closest (sort
                           (fn [left right]
                             (< 
                              (face-distance-from left center)
                              (face-distance-from right center)))
                           boundary-faces)]
    sorted-by-closest))

(defn select-boundary-face [boundary-faces]
  ;; Add some heurstics here:
  ;;
  ;; We want to prefer boundary-faces that are closer to the center of the polygon as a whole
  ;;
  ;; Later, we'll want to prefer boundary-faces that are not abutting another polygon
  (let [take-threshold (* (count boundary-faces) 0.5)]
    (if (> (q/random 1) 0.3)
      (do 
        (rand-nth boundary-faces))
      (do
        (rand-nth (take take-threshold (sort-by-distance-from-center boundary-faces))))
      )))


(defn any? [pred coll]
  (some? (some pred coll)))

(defn point-in-boundary? [point boundary-edges print]
  #_(when print
    (pprint boundary-edges))
  (let [converted-points (->> boundary-edges ;; {:start point1 :end point2}
                             (map vals)     ;; point1 point2
                             flatten
                             (map vals)     ;; [x1 y1] [x1 y1]
                             flatten)]      ;; x1 y1 x1 y1
    (poly/region-contains?
     (:x point) (:y point)
     converted-points)))

(defn overlapping-faces [faces polygons]
  (let [polygon-boundaries
        (->> polygons
             (map :faces)
             (map get-boundary))]
    (filter
     (fn [{:keys [point1 point2 point3]}]
       (any?
        (fn [b]
          (or (point-in-boundary? point1 b true)
              (point-in-boundary? point2 b true)
              (point-in-boundary? point3 b true)))
        polygon-boundaries))
     faces)))

(defn shrink-polygon [polygon rounds]
  (loop [round 0
         faces (:faces polygon)]
    (let [boundary-edges (get-boundary faces)
          boundary-faces (get-boundary-faces faces boundary-edges)]
      (if (< round rounds)
        (do
          (let [random-boundary-face (select-boundary-face boundary-faces)
                new-faces (clojure.set/difference (set faces) (set [(select-boundary-face boundary-faces)]))
                num-removed (- (count faces) (count new-faces))]
            (recur (+ num-removed round)
                   new-faces)))
        (merge polygon
               {:faces faces
                :round (+ (:round polygon) rounds)})))))

(defn done-shrinking? [polygon]
  (let [{:keys [round rounds]} polygon]
    (>= round rounds)))

(defn update-polygon [polygon]
  (if (done-shrinking? polygon)
    polygon
    (shrink-polygon polygon 2)))

(defn p-r [thing]
  (println thing)
  thing)

(defn find-free-points [grid polygons]
  (let [polygon-boundaries (->> polygons
                                (map (comp get-boundary :faces)))]
    (remove
     (fn [point]
       (some?
        (some
         (fn [b] (point-in-boundary? point b false))
         polygon-boundaries)))
     (enumerate-points grid))))

(defn bounding-boxes [grid scalar width height]
  (let [box (scale grid scalar)
        g-origin (:origin grid)
        g-width (:width grid)
        g-height (:height grid)]
    (for [x (range (:x g-origin) (+ (:x g-origin) g-width) width)
          y (range (:y g-origin) (+ (:y g-origin) g-height) height)]
      (translate box x y))))

(defn point-in-grid? [point grid]
  (let [origin-x (get-in grid [:origin :x])
        origin-y (get-in grid [:origin :y])
        farthest-x (+ origin-x (:width grid))
        farthest-y (+ origin-y (:height grid))]
    (and
     (>= (:x point) origin-x)
     (<= (:x point) farthest-x)
     (>= (:y point) origin-y)
     (<= (:y point) farthest-y))))

(defn max-free [grid free-points]
  (let [boxes (bounding-boxes grid 0.25 (q/random 60 100) (q/random 60 100))
        max (reduce
             (fn [max box]
               (let [free-points-in-box (filter #(point-in-grid? % box) free-points)
                     c (count free-points-in-box)]
                 (if (> c (:count max))
                   {:count c
                    :box box}
                   max))
               )
             {:count 0}
             boxes)]
    (:box max)))

(def free-points (atom nil))
(def g (atom nil))

#_(quil.applet/with-applet clj-polygons
  (let [state (initial-state)]
    (pprint (get-boundary (:faces (first (:polygons state)))))))

(defn find-free-region [{:keys [polygons grid]}]
  (let [free-points (find-free-points grid polygons)]
    (max-free grid free-points)))

(defn remove-overlapping-faces [polygon polygons]
  (loop [faces (:faces polygon)
         total-removed 0]
    (let [boundary-faces (->> faces
                              get-boundary
                              (get-boundary-faces faces))
          new-faces (clojure.set/difference
                     (set faces)
                     (overlapping-faces boundary-faces polygons))
          num-removed (- (count faces) (count new-faces))]
      (if (> num-removed 0)
        (recur new-faces
               (+ total-removed num-removed))
        (do
          (println total-removed)
          (merge polygon
                 {:faces new-faces
                  :round (+ (:round polygon) total-removed)}))))))

(defn make-polygon-in-free-region [{:keys [polygons grid] :as state}]
  (let [free-region (find-free-region {:polygons polygons :grid grid})
        polygon (make-polygon {:threshold 0.05 :grid free-region})
        faces-removed (remove-overlapping-faces polygon polygons)]
    faces-removed
    ))

(defn update-state [{:keys [polygons] :as state}]
  (let [all-done-shrinking false #_(every? done-shrinking? polygons)]
    (if all-done-shrinking
      (merge
       state
       {:polygons (conj polygons (make-polygon-in-free-region state))})
      (merge
       state
       {:polygons (map update-polygon polygons)}))))

(def s (atom nil))
 
(defn draw-state [{:keys [polygons grid] :as state}]
  #_(reset! s state)
  (let [bg-col {:r 31 :g 31 :b 30}]
    (q/background (:r bg-col) (:g bg-col) (:b bg-col))

    #_(let [boxes (bounding-boxes grid 0.25 100)]
      (doseq [box (take 26 boxes)]
        (q/stroke 0 255 0)
        (q/rect (get-in box [:origin :x]) (get-in box [:origin :y])
                (:width box)
                (:height box))))

    ;; Draw free points
    #_(let [free (find-free-points grid polygons)]
      (doseq [point free]
        (q/stroke 255 255 255)
        (q/point (:x point) (:y point))))
    (doseq [polygon polygons]

      
      ;; Draw triangles

      (doseq [{:keys [point1 point2 point3] :as face} (:faces polygon)]
        (let [{:keys [face-colors]} polygon
              color (get face-colors face)]

          #_(do
            (q/stroke (:r color) (:g color) (:b color))
            (q/fill (:r color) (:g color) (:b color)))
          (do
            (q/no-fill)
            (q/stroke 255 255 255))

          (q/triangle
           (:x point1) (:y point1)
           (:x point2) (:y point2)
           (:x point3) (:y point3)))
        )

      ;; Draw boundaries
      #_(let [boundary (get-boundary (:faces polygon))]
        (doseq [i (range (count boundary))]
          (let [edge (nth boundary i)]
            (do
              (q/no-fill)
              (q/stroke 255 0 0))
            (let [start (:start edge)
                  end (:end edge)]
              (q/fill 255 255 255)
              (q/text (str i) (:x start) (:y start))
              (q/no-fill)
              (q/line (:x start) (:y start) (:x end) (:y end))))))

      (q/save-frame "generated/iter1/####.png")
      ))
  )

(defn point-to-vec [point]
  [(:x point) (:y point)])

(q/defsketch clj-polygons
  :title "Polygons"
  :size [800 800]
  :settings
  (fn []
    (q/smooth 8))
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :mouse-pressed (fn [state event]
                   (reset! s state)
                   (if (q/looping?)
                     (do
                       (println "pausing")
                       (q/no-loop))
                     (do
                       (println "starting")
                       (q/start-loop)))
                   state)
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

