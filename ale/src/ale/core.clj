(ns ale.core
  (:require [clojure.java.io :as io]
            [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :as py :refer [py. py.. py.-]]
            [chickn.core :as chickn]
            [chickn.util :as util]
            [chickn.math :as math]))

(require-python
  'ale_py
  '(ale_py ALEInterface))

(def recording-dir "resources/recording")

(defn play-game [rom moves & {:keys [random-seed display-screen? sound? record? recording-dir] :or {random-seed 42
                                                                                                    display-screen? false
                                                                                                    sound? false
                                                                                                    record? false
                                                                                                    recording-dir recording-dir}}]
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
    (py. ale loadROM (str "resources/roms/" rom))
    (let [legal-moves (py. ale getLegalActionSet)]
      #_(println "legal moves" legal-moves)
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
  ;; 0 noop
  ;; 1 fire
  ;; 2 up
  ;; 3 right
  ;; 4 left
  ;; 5 down
  ;; ...
  (rand-nth [0 1 3 4] #_18))

(comment
  (play-game (repeatedly 10 rand-move) {:record? true
                                        :display-screen? false}))

(defn make-config [rom]
  #:chickn.core{:chromo-gen   #(repeatedly 1000 rand-move)
                :population-size     40
                :solved?      util/noop
                :fitness      (partial play-game rom)
                :monitor      util/noop
                :comparator   chickn/higher-is-better
                :reporter     util/simple-printer
                :parallel     false
                :selector    #:chickn.selector{:type        :chickn.selector/tournament
                                                 :rate        0.5
                                                 :random-func rand
                                                 :tour-size   8}
                :crossover    #:chickn.crossover{:type         :chickn.crossover/cut-crossover
                                                 :rate         0.5
                                                 :pointcuts    1
                                                 :random-point math/rnd-index
                                                 :random-func  rand
                                                 :rand-nth     rand-nth}
                :mutation     #:chickn.mutation{:type          :chickn.mutation/rand-mutation
                                                :rate          0.5
                                                :random-func   rand
                                                :mutation-func rand-move}
                :reinsertion     #:chickn.reinsertion{:type :chickn.reinsertion/elitist
                                                      :rate 0.1}})

;; TODO ensure

(defn reset-fs []
  (let [f (java.io.File. recording-dir)]
    (.mkdir f))
  (doseq [f (file-seq (java.io.File. recording-dir))]
    (when (.isFile f)
      (.delete f))))

(comment
  (reset-fs))

(comment
  (let [rom "demon_attack.bin"
        ga-cfg (make-config rom)
        population (chickn/init ga-cfg)
        iterations 2
        ;; evolve and find the best
        best-chromosome (:best-chromosome (chickn/evolve ga-cfg population iterations))]
    ;; delete existing files
    (reset-fs)
    ;; play the game again and record
    (play-game rom best-chromosome {:record? true :display-screen? false :sound? true})
    ;;
    #_(clojure.java.shell/sh)
    ))
