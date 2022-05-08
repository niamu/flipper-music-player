(ns fmf_player.spec
  (:require
   [clojure.spec.alpha :as s]))

(s/def :note/note
  (fn [s] (re-matches #"[A-G]|P" s)))

(s/def :note/duration (s/int-in 1 129))
(s/def :note/sharp? boolean?)
(s/def :note/octave (s/int-in 0 17))
(s/def :note/dots (s/int-in 0 17))

(s/def ::note
  (s/keys :req [:note/note]
          :opt [:note/duration
                :note/sharp?
                :note/octave
                :note/dots]))

(s/def :music/bpm integer?)
(s/def :music/duration :note/duration)
(s/def :music/octave :note/octave)

(s/def :music/notes
  (s/coll-of ::note))

(s/def ::music
  (s/keys :req [:music/bpm
                :music/duration
                :music/octave
                :music/notes]))

(def filetype "Flipper Music Format")
(def version 0)
