(ns fmf_player.parse-file
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   #?(:clj [clojure.java.io :as io])
   [fmf_player.spec :as spec]
   #?(:cljs [fmf_player.state :as state])))

(defn read-token
  [s token]
  (when (string/starts-with? s token)
    (subs s (+ (count token) 2))))

(defn read-token-string
  [s token]
  (read-token s token))

(defn read-token-number
  [s token]
  (let [s (read-token s token)
        n (and (re-find #"^\d+$" s)
               (#?(:clj parse-long
                   :cljs js/parseInt) s))]
    (and (number? n) n)))

(defn note->semitone
  [note]
  (case note
    "C" 0
    "D" 2
    "E" 4
    "F" 5
    "G" 7
    "A" 9
    "B" 11
    0))

(defn parse-notes
  [notes]
  (when notes
    (->> (string/split notes #",[\s+]?")
         (mapv (fn [note-s]
                 (let [d (some->> (re-matches #"^(\d+).*" note-s)
                                  second
                                  #?(:clj parse-long
                                     :cljs js/parseInt))
                       [note sharp? o dots]
                       (some->> (re-matches #"^([aA-gG|pP])(#|_)?(\d+)?(\.+)?.*"
                                            (subs note-s (count (str d))))
                                rest)]
                   (cond-> {:note/note (and note (string/upper-case note))}
                     d (assoc :note/duration d)
                     sharp? (assoc :note/sharp? true)
                     o (assoc :note/octave (#?(:clj parse-long
                                               :cljs js/parseInt) o))
                     dots (assoc :note/dots (count dots)))))))))

(defn parse
  [music]
  (if (s/valid? ::spec/music music)
    (->> (update music :music/notes
                 (fn [notes]
                   (mapv (fn [{:note/keys [note duration sharp? octave dots]
                              :as n}]
                           (cond-> (assoc n
                                          :note/semitone
                                          (if (= note "P")
                                            0xFF
                                            (+ (* (or octave
                                                      (:music/octave music))
                                                  12)
                                               (note->semitone note)
                                               (if sharp? 1 0)))
                                          :note/duration
                                          (or duration
                                              (:music/duration music)))
                             dots (assoc :note/dots dots)))
                         notes)))
         #?(:cljs (swap! state/state assoc :audio/current-file)))
    (throw (new #?(:clj Exception
                   :cljs js/Error)
                (str "File is invalid.\n\n"
                     (s/explain-str ::spec/music music))))))

(defn read-file
  [file]
  #?(:clj
     (with-open [reader (io/reader file)]
       (when (or (not= (read-token-string (.readLine reader) "Filetype")
                       spec/filetype)
                 (not= (read-token-number (.readLine reader) "Version")
                       spec/version))
         (throw (Exception. "Incorrect file format or version")))
       (let [music {:music/bpm (or (read-token-number (.readLine reader) "BPM")
                                   (throw (Exception. "BPM is missing")))
                    :music/duration (or (read-token-number (.readLine reader)
                                                           "Duration")
                                        (throw (Exception. "Duration is missing")))
                    :music/octave (or (read-token-number (.readLine reader)
                                                         "Octave")
                                      (throw (Exception. "Octave is missing")))
                    :music/notes (or (read-token-string (.readLine reader)
                                                        "Notes")
                                     (throw (Exception. "Notes is missing")))}
             music (update music :music/notes parse-notes)]
         (parse music)))
     :cljs
     (let [reader (new js/FileReader)]
       (set! (.-onload reader)
             (fn []
               (let [result (string/split-lines (.-result reader))
                     [filetype version bpm duration octave notes] result
                     _ (when (or (not= (read-token-string filetype "Filetype")
                                       spec/filetype)
                                 (not= (read-token-number version "Version")
                                       spec/version))
                         (throw (js/Error. "Incorrect file format or version")))
                     music
                     {:music/bpm (or (read-token-number bpm "BPM")
                                     (throw (js/Error. "BPM is missing")))
                      :music/duration (or (read-token-number duration "Duration")
                                          (throw
                                           (js/Error. "Duration is missing")))
                      :music/octave (or (read-token-number octave "Octave")
                                        (throw (js/Error. "Octave is missing")))
                      :music/notes (or (read-token-string notes "Notes")
                                       (throw (js/Error. "Notes is missing")))}
                     music (update music :music/notes parse-notes)]
                 (parse music))))
       (when file
         (.readAsText reader file)))))
