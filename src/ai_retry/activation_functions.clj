(ns ai-retry.activation-functions)

(defrecord Activation-Pair [activation derivative])



(def step
  (->Activation-Pair
    #(int (Math/signum ^double %))
    (constantly 1))) ; TODO: Probably wrong. Non-differentiable?

(declare sigmoid-f)
(def sigmoid
  (->Activation-Pair
    #(sigmoid-f % Math/E)
    #(* % (- 1 %))))



(defn sigmoid-f [n scale]
  (/ 1 (+ 1 (Math/pow scale (- n)))))