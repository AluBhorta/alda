(ns alda.lisp.model.attribute
  (:require [taoensso.timbre       :as    log]
            [alda.lisp.model.event :refer (update-score)]))

; This is a map of keywords to Attribute records.
; When an attribute change event occurs in a score, we look up its attribute
; (a keyword) in this map and use the data about this name to update the
; appropriate attribute of the current instruments.
;
; This map gets filled in with Alda's various attributes via the `defattribute`
; macro in alda.lisp.attributes.
(def ^:dynamic *attribute-table* {})

(defn apply-attribute
  "Given an instrument map, a keyword representing an attribute, and a value,
   returns the updated instrument with that attribute update applied."
  [inst attr val]
  (if-let [{:keys [transform-fn kw-name]} (*attribute-table* attr)]
    (let [old-val (kw-name inst)
          new-val ((transform-fn val) old-val)]
      (when (not= old-val new-val)
        (log/debug (format "%s %s changed from %s to %s."
                           (:id inst) (str kw-name) old-val new-val)))
      (assoc inst kw-name new-val))
    (throw (Exception. (str attr " is not a valid attribute.")))))

(defmethod update-score :attribute-change
  [{:keys [current-instruments] :as score} {:keys [attr val] :as attr-change}]
  (update score :instruments
          into {} map (fn [[id inst]]
                        [id (if (contains? current-instruments id)
                              (apply-attribute inst attr val)
                              inst)])))

(defn set-attribute
  "Public fn for setting attributes in a score.
   e.g. (set-attribute :tempo 100)"
  [attr val]
  {:event-type :attribute-change
   :attr       attr
   :val        val})

(defn set-attributes
  "Convenience fn for setting multiple attributes at once.
   e.g. (set-attributes :tempo 100 :volume 50)"
  [& attrs]
  (for [[attr val] (partition 2 attrs)]
    (set-attribute attr val)))

; (defn snapshot
;   [instrument]
;   (*instruments* instrument))

; (defn load-snapshot
;   [instrument snapshot]
;   (alter-var-root #'*instruments* assoc instrument snapshot))
