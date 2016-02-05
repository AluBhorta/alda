(ns alda.lisp.events.note
  (:require [alda.lisp.attributes     :refer :all]
            [alda.lisp.model.duration :refer (duration)]
            [alda.lisp.model.event    :refer (add-event
                                              set-current-offset
                                              set-last-offset)]
            [alda.lisp.model.marker   :refer ($current-marker)]
            [alda.lisp.model.offset   :refer ($current-offset
                                              offset+)]
            [alda.lisp.model.records  :refer (map->Note)]
            [alda.lisp.score.context  :refer (*beats-tally*
                                              *current-instruments*)]
            [taoensso.timbre          :as    log]))

(defmethod update-score :note
  [{:keys [beats-tally current-instruments time-scaling] :as score}
   {:keys [beats ms] :as event}]
  (if beats-tally
    (update score :beats-tally + beats)
    (update score :instruments
            into {}
            map (fn [[id {:keys [duration tempo current-offset last-offset
                                 current-marker] :as inst}]]
                  (if (contains? current-instruments id)
                    (let [rest-duration (calculate-duration beats
                                                            tempo
                                                            time-scaling
                                                            ms)]
                      (log/debug (format "%s rests at %s + %s for %s ms."
                                         id
                                         current-marker
                                         (int (:offset last-offset))
                                         (int rest-duration)))
                      [id (-> inst
                              (assoc :duration beats)
                              (assoc :last-offset current-offset)
                              (assoc :current-offset (offset+ current-offset
                                                              rest-duration)))])
                    [id inst])))))

(defn note
  "Causes every instrument in :current-instruments to play a note at its
   :current-offset for the specified duration.

   If no duration is specified, the note is played for the instrument's own
   internal duration, which will be the duration last specified on a note or
   rest in that instrument's part."
  ([pitch-fn]
    "TODO: make this fn construct a note event map"
    (note* instrument
           pitch-fn
           false))
  "TODO:
    * add note event to each current instrument
    * adjust current/last offset of each current instrument
    * set duration of each current instrument")

(defn note*
  ([instrument pitch-fn]
   {:pre [(fn? pitch-fn)]}
    (note* instrument
           pitch-fn
           (duration ($duration instrument))
           false))
  ([instrument pitch-fn arg3]
    (cond ; arg3 could be a duration or :slur
      (map? arg3)    (note* instrument
                            pitch-fn
                            arg3
                            false)
      (= :slur arg3) (note* instrument
                            pitch-fn
                            (duration ($duration instrument))
                            true)))
  ([instrument pitch-fn {:keys [duration-fn beats slurred]} slur?]
    (let [quant          (if (or slur? slurred) 1.0 ($quantization instrument))
          note-duration  (duration-fn ($tempo instrument))
          event          (when-not *beats-tally*
                           (map->Note
                             {:offset       ($current-offset instrument)
                              :instrument   instrument
                              :volume       ($volume instrument)
                              :track-volume ($track-volume instrument)
                              :panning      ($panning instrument)
                              :midi-note    (pitch-fn ($octave instrument)
                                                      ($key-signature instrument)
                                                      :midi true)
                              :pitch        (pitch-fn ($octave instrument)
                                                      ($key-signature instrument))
                              :duration     (* note-duration quant)}))]
      (if event
        (do
          (add-event instrument event)
          (set-last-offset instrument ($current-offset instrument))
          (set-current-offset instrument (offset+ ($current-offset instrument)
                                                  note-duration))
          (log/debug (format "%s plays at %s + %s for %s ms, at %.2f Hz."
                             instrument
                             ($current-marker instrument)
                             (int (:offset (:offset event)))
                             (int (:duration event))
                             (:pitch event)))
          event)
        (alter-var-root #'*beats-tally* + beats)))))

