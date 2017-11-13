(ns chicknism.core
    (:require [reagent.core :as reagent :refer [atom]]
              [chickn.core :as chickn]
              [chickn.util :refer [noop simple-printer]]
              [chickn.math :refer [rnd-index]]))

(enable-console-print!)

;; ------------------------
;; state + state + helpers

(declare app-state)

(defn rand-chromo []
  (let [chickn-cities (mapv (fn [[n [x y]]] {:name n :x x :y y}) (-> @app-state :cities))]
    (shuffle chickn-cities)))

(defn dist-squared [[{x1 :x y1 :y} {x2 :x y2 :y}]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn fitness [cities]
  (apply + (map dist-squared (partition 2 1 cities))))


(defonce app-state (atom {:genotype {}
                          :cities {}
                          :path []
                          :token nil
                          :state :reset
                          :cities-cnt 50
                          :cfg #:chickn.core{:chromo-gen rand-chromo
                                             :pop-size     100
                                             :terminated? noop
                                             ;:monitor     monitor
                                             :monitor     noop
                                             :fitness      fitness
                                             :comparator   chickn/lower-is-better
                                             :reporter     simple-printer
                                             :selectors  [#:chickn.selectors{:type        :chickn.selectors/best
                                                                             :elit        true
                                                                             :rate        0.1
                                                                             :random-func rand}
                                                          #:chickn.selectors{:type        :chickn.selectors/roulette
                                                                             :rate        0.3
                                                                             :random-func rand}]
                                             :operators    [#:chickn.operators{:type         :chickn.operators/ordered-crossover
                                                                               :rate         0.3
                                                                               :random-point rnd-index
                                                                               :rand-nth     rand-nth}
                                                            #:chickn.operators{:type        :chickn.operators/swap-mutation
                                                                               :rate        0.1
                                                                               :rand-nth    rnd-index
                                                                               :random-func rand}]}}))

;; chickn stuff

(defn evolve-once []
  (let [[_ new-gen] (chickn.core/evolve (-> @app-state :cfg) (-> @app-state :genotype))
        new-path (mapv :name (-> new-gen :best-chromo))]
    (swap! app-state assoc :genotype new-gen :path new-path)))

(defn evolve []
  (swap! app-state assoc :token (js/setInterval evolve-once 1) :state :started))

(defn stop []
  (js/clearInterval (-> @app-state :token))
  (swap! app-state assoc :token nil :state :stopped))


;; ------------
;; webapp stuff

(def width-max 850)
(def height-max 450)

(defn create-cities [n]
  (into [] (take n  (map #(keyword (str "c" %)) (range)))))

(defn create-city-points [c]
  (let [x (+ 10 (rand-int (- width-max 10)))
        y (+ 10 (rand-int (- height-max 10)))]
    {c [x y]}))

(defn reset []
  (swap! app-state assoc :state :reset :genotype {} :cities {} :path []))

(defn init []
  (let [cities (create-cities (-> @app-state :cities-cnt))
        _ (swap! app-state assoc :cities (reduce (fn [a c] (merge (create-city-points c) a)) {} cities))
        _ (swap! app-state assoc :path cities)
        genotype (chickn.core/init (-> @app-state :cfg))]
    (swap! app-state assoc :genotype genotype :state :stopped)))

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


(defn slider [label param-path min max rate]
  (let [val (get-in (-> @app-state) param-path)
        val (if rate (* val 100) val)]
    [:div (str label " " val (if rate " %" ""))
     [:input {:type "range" :value val :min min :max max
              :style {:width "100%"}
              :on-change (fn [e]
                           (swap! app-state assoc-in param-path
                                  (let [val (int (.. e -target -value))]
                                    (if rate
                                      (/ val 100)
                                      val))))}]
     [:br]
     [:br]]))

(defn control-panel []
  [:div
   [:input {:type "button" :value "Reset"
            :disabled (not (some (hash-set (-> @app-state :state)) [:stopped]))
            :on-click #(reset)}]
   [:input {:type "button" :value "Init"
            :disabled (not (some (hash-set (-> @app-state :state)) [:reset]))
            :on-click #(init)}]
   [:input {:type "button" :value "Evolve once"
            :disabled (some (hash-set (-> @app-state :state)) [:started :reset])
            :on-click #(evolve-once)}]
   [:input {:type "button" :value "Start"
            :disabled (some (hash-set (-> @app-state :state)) [:started :reset])
            :on-click #(evolve)}]
   [:input {:type "button" :value "Stop"
            :disabled (not (some (hash-set (-> @app-state :state)) [:started]))
            :on-click #(stop)}]])

(defn cfg-controls []
  [:div {:style {:width width-max}}
   [slider "Cities " [:cities-cnt] 10 300]
   [slider "Population size" [:cfg :chickn.core/pop-size] 10 1000]
   [slider "Elitism rate" [:cfg :chickn.core/selectors 0 :chickn.selectors/rate] 0 100 :rate]
   [slider "Crossover rate" [:cfg :chickn.core/operators 0 :chickn.operators/rate] 0 100 :rate]
   [slider "Mutation rate" [:cfg ::chickn/operators 1 :chickn.operators/rate] 0 100 :rate]])

(defn div-with-canvas []
  (let [dom-node (reagent/atom nil)]
    (reagent/create-class
      {:component-did-update
       (fn [this]
         (draw-canvas-contents (.-firstChild @dom-node)))

       :component-did-mount
       (fn [this]
         (reset! dom-node (reagent/dom-node this)))

       :reagent-render
       (fn []
         @app-state
         [:div.with-canvas
          [:canvas (if-let [node @dom-node]
                     {:width (.-clientWidth node)
                      :height (.-clientHeight node)})]


          ])})))

(defn wrapper []
  [:div
   [:p "Solving the Traveling Salesman Problem using the "
    [:a {:href "https://github.com/kongeor/chickn"} "chickn"] " library"]
   [control-panel]
   [:div
    [:p "Iteration: " (-> @app-state :genotype :iteration)]
    [:p "Best fitness: " (-> @app-state :genotype :best-fitness)]]
   [div-with-canvas]
   [cfg-controls]]
  )

(defn on-window-resize [evt]
  (reset! window-width (.-innerWidth js/window)))

(reagent/render [wrapper]
                (. js/document (getElementById "app"))
                (.addEventListener js/window "resize" on-window-resize))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

