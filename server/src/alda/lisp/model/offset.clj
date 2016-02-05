(ns alda.lisp.model.offset
  (:require [alda.lisp.model.records]
            [alda.util       :refer (=%)]
            [taoensso.timbre :as    log])
  (:import [alda.lisp.model.records AbsoluteOffset RelativeOffset]))

(defprotocol Offset
  (absolute-offset [this score] "Returns the offset in ms from the start of the score.")
  (offset+ [this bump] "Returns a new offset bump ms later."))

(extend-protocol Offset
  Number
  (absolute-offset [x _] x)
  (offset+ [x bump] (+ x bump))

  AbsoluteOffset
  (absolute-offset [this _]
    (:offset this))
  (offset+ [this bump]
    (AbsoluteOffset. (+ (:offset this) bump)))

  RelativeOffset
  (absolute-offset [this {:keys [events] :as score}]
    (if-let [marker-offset (-> (events (:marker this)) :offset)]
      (+ (absolute-offset marker-offset score) (:offset this))
      (log/warn "Can't calculate offset - marker" (str \" (:marker this) \")
                "does not have a defined offset.")))
  (offset+ [this bump]
    (RelativeOffset. (:marker this) (+ (:offset this) bump))))

(defn offset=
  "Convenience fn for comparing absolute/relative offsets."
  [& offsets]
  (if (and (every? #(instance? RelativeOffset %) offsets)
           (apply = (map :marker offsets)))
    (apply =% (map :offset offsets))
    (apply =% (map absolute-offset offsets))))

;;;

(defn $current-offset
  "Get the :current-offset of an instrument."
  ([score] ($current-offset score (-> score :current-instruments first)))
  ([score instrument] (-> score :instruments instrument :current-offset)))

(defn $last-offset
  "Get the :last-offset of an instrument."
  ([score] ($last-offset score (-> score :current-instruments first)))
  ([score instrument] (-> score :instruments instrument :last-offset)))

(defn instruments-all-at-same-offset
  "If all of the :current-instruments are at the same absolute offset, returns
   that offset. Returns nil otherwise.

   (Returns 0 if there are no instruments defined yet, e.g. when placing a
    marker or a global attribute at the beginning of a score.)"
  [{:keys [current-instruments] :as score}]
  (if (empty? current-instruments)
    (AbsoluteOffset. 0)
    (let [offsets (map (comp absolute-offset $current-offset)
                       current-instruments)]
      (when (apply == offsets)
        (AbsoluteOffset. (first offsets))))))
