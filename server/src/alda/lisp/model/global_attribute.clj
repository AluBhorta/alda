(ns alda.lisp.model.global-attribute
  (:require [alda.lisp.model.attribute :refer (apply-attribute)]
            [alda.lisp.model.event     :refer (update-score)]
            [alda.lisp.model.offset    :refer (absolute-offset
                                               instruments-all-at-same-offset)]
            [taoensso.timbre           :as    log]))

(defmethod update-score :global-attribute-change
  [score {:keys [attr val] :as event}]
  (if-let [offset (instruments-all-at-same-offset score)]
    (let [abs-offset (absolute-offset offset score)]
      (log/debugf "Set global attribute %s %s at offset %d."
                  attr val (int abs-offset))
      (update-in score [:global-attributes abs-offset]
                 (fnil conj []) [attr val]))
    (throw (Exception.
             (str "Can't set global attribute " attr " to " val " - offset "
                  "unclear. There are multiple instruments active with "
                  "different time offsets.")))))

(defn global-attribute
  "Public fn for setting global attributes in a score.
   e.g. (global-attribute :tempo 100)"
  [attr val]
  {:event-type :global-attribute-change
   :attr       attr
   :val        val})

(defn global-attributes
  "Convenience fn for setting multiple global attributes at once.
   e.g. (global-attributes :tempo 100 :volume 50)"
  [& attrs]
  (for [[attr val] (partition 2 attrs)]
    (global-attribute attr val)))

(defmethod update-score :apply-global-attributes
  [{:keys [global-attributes instruments current-instruments] :as score} _]
  (update score :instruments
          into {} map (fn [[id {:keys [current-offset last-offset] :as inst}]]
                        (if (contains? current-instruments id)
                          (let [global-attrs (->> global-attributes
                                                  (filter (fn [[offset attrs]]
                                                            (<= last-offset
                                                                offset
                                                                current-offset)))
                                                  (mapcat (fn [[offset attrs]]
                                                            attrs)))]
                            [id (reduce (fn [inst [attr val]]
                                          (apply-attribute inst attr val))
                                        inst
                                        global-attrs)])
                          inst))))

(defn apply-global-attributes
  "For each instrument in :current-instruments, looks between the instrument's
   :last-offset and :current-offset and applies any global attribute changes
   occurring within that window."
  []
  {:event-type :apply-global-attributes})
