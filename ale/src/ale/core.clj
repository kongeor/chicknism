(ns ale.core
  (:require [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :as py :refer [py. py.. py.-]]))

(require-python
  'ale_py
  '(ale_py ALEInterface))

(def ale (ale_py/ALEInterface))

(py. ale setInt "random_seed" 123)
(py. ale setBool "display_screen" true)
(py. ale setBool "sound" true)

(def record-path "record")

(py. ale setString "record_screen_dir" record-path)
(py. ale setString "record_sound_filename" (str record-path "/sound.wav"))
(py. ale setInt "fragsize" 64)


(py. ale loadROM "roms/tetris.bin")

(loop [reward 0]
  (if (py. ale game_over)
    reward
    (let [action (rand-nth (py. ale getLegalActionSet))
          reward' (py. ale act action)]
      (println "action " action " reward " reward')
      (recur (+ reward reward')))))

(rand-nth (py. ale getLegalActionSet))

(py. ale getLegalActionSet)

(py. ale game_over)

