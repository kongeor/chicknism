(ns chicknism.core
    (:require [reagent.core :as reagent :refer [atom]]
              [chickn.core :as chickn]))

(enable-console-print!)


;; state

(defonce app-state (atom {:cities {}
                          :path []}))

;; chickn stuff

(def cities [{:name :A :x 1 :y 1}
             {:name :B :x 5 :y 1}
             {:name :C :x 10 :y 1}
             {:name :D :x 10 :y 5}
             {:name :E :x 10 :y 10}
             {:name :F :x 5 :y 10}
             {:name :G :x 1 :y 10}
             {:name :H :x 1 :y 5}])

(defn dist-squared [[{x1 :x y1 :y} {x2 :x y2 :y}]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn fitness [cities]
  (apply + (map dist-squared (partition 2 1 cities))))

#_(defn init-pop [n]
    (repeatedly n #(shuffle cities)))

(defn init-pop [n]
  (let [chickn-cities (mapv (fn [[n [x y]]] {:name n :x x :y y}) (-> @app-state :cities))]
    (repeatedly n #(shuffle chickn-cities))))

(defn rnd-index [coll]
  (int (* (rand) (count coll))))

(defn data-reporter [{:keys [iteration best-fitness best-chromo]}]
  (let [best-path (mapv :name best-chromo)]
    (println "Iteration: " iteration " Best fitness: " best-fitness)
    (if (= (mod iteration 20) 0)
      (swap! app-state assoc :path best-path))))

(defn evolve []
  (let [cfg #:chickn.core{:init-pop     #(chickn/raw-pop->pop (init-pop 30))
                          :pop-size     30
                          :rand-nth     rand-nth
                          ;:random-point rnd-index
                          :elitism-rate 0.1
                          :terminated? (fn [c])
                          :fitness      fitness
                          :comparator   chickn/ascending
                          :reporter     data-reporter
                          :selector     #:chickn.selectors{:type        :chickn.selectors/roulette
                                                           :random-func rand}
                          :operators    [#:chickn.operators{:type         :chickn.operators/ordered-crossover
                                                            :rate         0.3
                                                            :random-point (partial rnd-index (range 8))
                                                            :rand-nth     rand-nth}
                                         #:chickn.operators{:type        :chickn.operators/swap-mutation
                                                            :rate        0.1
                                                            :rand-nth    rnd-index
                                                            :random-func rand}]}]
    (select-keys (chickn/evolve* cfg 500) [:solved? :time])))

;; webapp stuff

(def width-max 760)
(def height-max 650)

(defn create-cities [n]
  (into [] (take n  (map #(keyword (str "c" %)) (range)))))

(defn create-city-points [c]
  (let [x (+ 10 (rand-int (- width-max 10)))
        y (+ 10 (rand-int (- height-max 10)))]
    {c [x y]}))

(defn init [n]
  (let [cities (create-cities n)]
    (swap! app-state assoc :cities (reduce (fn [a c] (merge (create-city-points c) a)) {} cities))
    (swap! app-state assoc :path cities)))

(def window-width (reagent/atom nil))

(defn draw-city [canvas city]
  (let [[x y] (-> @app-state :cities city)
        d 3
        ctx (.getContext canvas "2d")]
    (.beginPath ctx)
    (.arc ctx x y d 0 (* 2 js/Math.PI))
    (.fill ctx)))

(defn draw-line [canvas c1 c2]
  (let [ctx (.getContext canvas "2d")
        [x1 y1] (-> @app-state :cities c1)
        [x2 y2] (-> @app-state :cities c2)]
    (.beginPath ctx)
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2)
    (.stroke ctx)))

(defn draw-canvas-contents [canvas]
  (let [ctx (.getContext canvas "2d")
        path (@app-state :path)
        path-pairs (partition 2 1 (conj path (first path)))]
    (.clearRect ctx 0 0 width-max height-max)
    (mapv (fn [[c1 c2]]                                     ;hack!
            (draw-city canvas c1)
            (draw-city canvas c2)
            (draw-line canvas c1 c2)) path-pairs)))


(defn path-shuffler []
  (js/setInterval
    #(swap! app-state update-in [:path] shuffle) 100))

(defn div-with-canvas []
  (let [dom-node (reagent/atom nil)]
    (reagent/create-class
      {:component-did-update
       (fn [this]
         (draw-canvas-contents (.-firstChild @dom-node)))

       :component-did-mount
       (fn [this]
         (init 50)                                           ; hack!
         #_(path-shuffler)
         (reset! dom-node (reagent/dom-node this))
         )

       :reagent-render
       (fn []
         @app-state
         [:div.with-canvas
          [:canvas (if-let [node @dom-node]
                     {:width (.-clientWidth node)
                      :height (.-clientHeight node)})]
          [:input {:type "button" :value "Start!"
                   :on-click #(evolve)}]])})))


(defn starter []
  (js/setInterval #(println "secondo") 1000))

(defn div-with-canvas2 []
  (let [dom-node (reagent/atom nil)]
    (fn []
      (starter)
      [:div "yo"])))

(defn on-window-resize [evt]
  (reset! window-width (.-innerWidth js/window)))

(reagent/render [div-with-canvas]
                (. js/document (getElementById "app"))
                (.addEventListener js/window "resize" on-window-resize))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

