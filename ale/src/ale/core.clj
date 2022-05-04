(ns ale.core
  (:require [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :as py :refer [py. py.. py.-]]
            [chickn.core :as chickn]
            [chickn.util :as util]
            [chickn.math :as math]))

(require-python
  'ale_py
  '(ale_py ALEInterface))

(defn play-game [moves & {:keys [random-seed display-screen? sound? record? recording-dir] :or {random-seed 42
                                                                                                display-screen? false
                                                                                                sound? false
                                                                                                record? false
                                                                                                recording-dir "resources/recording"}}]
  (let [ale (ale_py/ALEInterface)]
    (py. ale setInt "random_seed" random-seed)
    (when display-screen?
      (py. ale setBool "display_screen" display-screen?))
    (when sound?
      (py. ale setBool "sound" sound?))
    (when record?
      (py. ale setString "record_screen_dir" recording-dir)
      (py. ale setString "record_sound_filename" (str recording-dir "/sound.wav"))
      (py. ale setInt "fragsize" 64))
    (py. ale loadROM "resources/roms/demon_attack.bin")
    (let [legal-moves (py. ale getLegalActionSet)]
      (loop [i      0
             reward 0]
        (if (or
              (py. ale game_over)
              (>= i (count moves)))
          reward
          (let [m (nth moves i)
                action (nth legal-moves m)
                reward' (py. ale act action)]
            (recur (inc i) (+ reward reward'))))))))

(defn rand-move []
  (rand-int 6 #_18))

(comment
  (play-game (repeatedly 1000 rand-move) {:record? false
                                          :display-screen? true
                                          }))

(def ga-cfg
  #:chickn.core{:chromo-gen   #(repeatedly 5000 rand-move)
                :pop-size     40
                :elitism-rate 0.1
                :terminated?  util/noop
                :monitor      util/noop
                :fitness      play-game
                :comparator   chickn/higher-is-better
                :reporter     util/simple-printer
                :parallel     false
                :selectors    [#:chickn.selectors{:type        :chickn.selectors/best
                                                  :elit        true
                                                  :rate        0.1
                                                  :random-func rand}
                               #:chickn.selectors{:type        :chickn.selectors/tournament
                                                  :rate        0.5
                                                  :random-func rand
                                                  :tour-size   8}]
                :operators    [#:chickn.operators{:type         :chickn.operators/cut-crossover
                                                  :rate         0.5
                                                  :pointcuts    1
                                                  :random-point math/rnd-index
                                                  :random-func  rand
                                                  :rand-nth     rand-nth}
                               #:chickn.operators{:type          :chickn.operators/rand-mutation
                                                  :rate          0.5
                                                  :random-func   rand
                                                  :mutation-func rand-move}]})

(comment
  (let [genotype (chickn/init ga-cfg)]
    (-> (chickn/evolve ga-cfg genotype 50)
      :genotype
      :best-chromo
      (play-game {:record? true :sound? true}))))
