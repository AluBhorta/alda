(ns alda.lisp.events.rest
  (:require [alda.lisp.model.duration :refer (duration calculate-duration)]
            [alda.lisp.model.event    :refer (update-score)]
            [alda.lisp.model.offset   :refer (offset+)]
            [taoensso.timbre          :as    log]))

(defmethod update-score :rest
  [{:keys [beats-tally current-instruments time-scaling] :as score}
   {:keys [beats ms] :as event}]
  (if beats-tally
    (update score :beats-tally + beats)
    (update score :instruments
            into {}
            map (fn [[id {:keys [tempo current-offset current-marker] :as inst}]]
                  (if (contains? current-instruments id)
                    (let [rest-duration (if (or beats ms)
                                          (calculate-duration beats
                                                              tempo
                                                              time-scaling
                                                              ms)
                                          (duration (:duration inst)))]
                      (log/debug (format "%s rests at %s + %s for %s ms."
                                         id
                                         current-marker
                                         (int (:offset current-offset))
                                         (int rest-duration)))
                      [id (-> inst
                              (assoc :duration beats)
                              (assoc :last-offset current-offset)
                              (assoc :current-offset (offset+ current-offset
                                                              rest-duration)))])
                    [id inst])))))

(defn pause
  "Causes every instrument in :current-instruments to rest (not play) for the
   specified duration.

   If no duration is specified, each instrument will rest for its own internal
   duration, which will be the duration last specified on a note or rest in
   that instrument's part."
  [& [{:keys [beats ms] :as dur}]]
   {:event-type :rest
    :beats      beats
    :ms         ms})

