(ns fmf_player.core
  (:require
   [fmf_player.parse-file :as parse-file]
   [fmf_player.state :as state]))

(def note-c4 261.63)
(def note-c4-semitone (* 4 12))
(def two-pow-twelfth-root 1.059463094359)

(defn new-audio
  []
  (let [audio-context (or (.. js/window -AudioContext)
                          (.. js/window -webkitAudioContext))
        audio-context (new audio-context)
        gain-node (doto (.createGain audio-context)
                    (.connect (.-destination audio-context)))]
    (set! (.. gain-node -gain -value) 1)
    {:audio/context audio-context
     :audio/gain-node gain-node
     :audio/current-note-index 0
     :audio/falloff-interval (js/setInterval
                              (fn []
                                (let [volume (or (.. gain-node -gain -value) 1)]
                                  (set! (.. gain-node -gain -value)
                                        (* volume 0.9945679))))
                              2)
     :audio/playing? false}))

(defn stop
  []
  (when (get @state/state :audio/context)
    (.close (get @state/state :audio/context)))
  (js/clearInterval (get @state/state :audio/falloff-interval))
  (swap! state/state merge (new-audio)))

(defn play-note!
  []
  (let [total-notes (count (:audio/notes @state/state))
        current-note-index (get @state/state :audio/current-note-index 0)
        note (get-in @state/state
                     [:audio/notes
                      current-note-index])
        next-note (get-in @state/state
                          [:audio/notes
                           (if (>= (inc current-note-index)
                                   total-notes)
                             0
                             (inc current-note-index))])
        duration (+ (:note/duration note)
                    (if (= note next-note)
                      (get next-note :note/duration 0)
                      0))
        gain-node (get @state/state :audio/gain-node)
        oscillator (doto (.createOscillator (:audio/context @state/state))
                     (.connect gain-node))]
    (if (and (:audio/playing? @state/state) note)
      (do (swap! state/state update
                 :audio/current-note-index
                 (fn [idx]
                   (cond
                     (and (= note next-note)
                          (>= (inc current-note-index)
                              total-notes)) 1
                     (= note next-note) (+ idx 2)
                     (>= (inc current-note-index) total-notes) 0
                     :else (inc idx))))
          (set! (.-type oscillator) "sawtooth")
          (set! (.. gain-node -gain -value) 1)
          (set! (.. oscillator -frequency -value)
                (* note-c4
                   (.pow js/Math
                         two-pow-twelfth-root
                         (- (:note/semitone note) note-c4-semitone))))
          (.start oscillator)
          (.stop oscillator (+ (.-currentTime (:audio/context @state/state))
                               duration))
          (js/setTimeout play-note! (* duration 1000)))
      (stop))))

(defn play
  []
  (when-let [{:music/keys [bpm duration notes] :as music}
             (:audio/current-file @state/state)]
    (stop)
    (swap! state/state assoc
           :audio/playing? true
           :audio/notes (->> notes
                             (mapv (fn [{:keys [note/dots] :as note}]
                                     (assoc note
                                            :note/duration
                                            (reduce (fn [accl _]
                                                      (+ accl
                                                         (/ accl 2)))
                                                    (/ (* 60 4)
                                                       bpm
                                                       (get note :note/duration
                                                            duration))
                                                    (range dots))))))))
  (js/setTimeout play-note! 250))

(let [file-input (.. js/document (querySelector "input#file"))
      play-button (.. js/document (querySelector "button#play"))
      stop-button (.. js/document (querySelector "button#stop"))]
  (set! (.-onchange file-input) #(when-let [file (first (.-files file-input))]
                                   (parse-file/read-file file)))
  (set! (.-onclick stop-button) #(stop))
  (set! (.-onclick play-button) #(play)))
