(ns ai-retry.activation-functions)

(defrecord Activation-Pair [activation derivative])

(def step (->Activation-Pair
               #(int (Math/signum ^double %))
               (constantly 1))) ; TODO: Probably wrong. Non-differentiable?

