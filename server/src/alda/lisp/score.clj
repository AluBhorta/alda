(ns alda.lisp.score
  (:require [alda.lisp.model.event   :refer (update-score)]
            [alda.lisp.model.offset  :refer (absolute-offset)]
            [alda.lisp.model.records :refer (->AbsoluteOffset)]))

(defn new-score
  []
  {:events              {:start {:offset (->AbsoluteOffset 0), :events []}}
   :time-scaling        1 ; can be altered inside of a CRAM expression
   :beats-tally         nil ; used when tallying beats in a CRAM expression
   :global-attributes   {} ; a map of offsets to the global attribute changes
                           ; that occur (for all instruments) at each offset
   :current-instruments #{}
   :instruments         {}
   :nicknames           {}})

(defn continue
  "Continues the score represented by the score map `score`, evaluating the
   events in `body` and returning the completed score."
  [score & body]
  (reduce update-score score body))

(defn continue!
  "Convenience function for dealing with Alda scores stored in atoms.

   (continue! my-score
     (part 'bassoon'
       (note (pitch :c))))

   is short for:

   (apply swap! my-score continue
     (part 'bassoon'
       (note (pitch :c))))"
  [score-atom & body]
  (apply swap! score-atom continue body))

(defn score
  "Initializes a new score, evaluates the events contained in `body` (updating
   the score accordingly) and returns the completed score.

   A score and its evaluation context are effectively the same thing. This
   means that an evaluated score can be used as an input to `continue-score`"
  [& body]
  (apply continue (new-score) body))
