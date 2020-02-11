(defproject clj-polygons "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [aysylu/loom "1.0.2"]
                 [trystan/delaunay-triangulation "1.0.1"]
                 [quil "3.1.0"]]
  :resource-paths ["resources/mesh.jar"
                   "resources/quickhull3d.1.4.jar"])
