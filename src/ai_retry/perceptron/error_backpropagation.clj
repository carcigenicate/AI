(ns ai-retry.perceptron.error-backpropagation
  (:require [ai-retry.perceptron.perceptron :as p]
            [ai-retry.perceptron.layer :as l]
            [ai-retry.helpers.number-helpers :as nh]
            [ai-retry.activation-functions :as af]
            [ai-retry.perceptron.learning-sets :as ls]

            [helpers.general-helpers :as g]))

(def error-learning-rate-multiple 100)

(defn output-node-error [expected-output actual-output derivative]
  (* (derivative actual-output)
     (- expected-output actual-output)))

(defn hidden-node-error
  "Calculates the error for a hidden node.
  next-layer-error-map should be a map of {error weight-to-current-node, ...}"
  [activation derivative next-layer-error-map]
  (* (derivative activation)
     (nh/sum
       (map (fn [[error weight]] (* error weight))
            next-layer-error-map))))

(defn weight-adjustment [edge-source-activation edge-target-error]
  (* edge-source-activation edge-target-error))

(defn adjust-weight [old-edge-weight edge-source-activation edge-target-error learning-rate]
  (+ old-edge-weight
     (* (weight-adjustment edge-source-activation edge-target-error)
        learning-rate)))

(defn adjust-weights
  "Assumes (= (count source-activations) (count old-weights))"
  [old-weights source-activations target-error learning-rate]
  (mapv (fn [w a]
          (adjust-weight w a target-error learning-rate))
        old-weights source-activations))

(defn adjustment-loop [perceptron learning-set iterations]
  (let [deriv (:derivative (:activation-pair perceptron))
        cycled-sets (cycle learning-set)
        print-every (/ iterations 100.0)]

    (loop [i 0
           acc-p perceptron
           [[input output] & rest-sets] cycled-sets]

      (let [biased-input (p/biased-input perceptron input)
            fired (p/fire acc-p input)
            fired-act (:last-activation fired)
            error (output-node-error output fired-act deriv)
            l-rate (* (Math/abs ^double error) error-learning-rate-multiple)
            p' (update fired :weights #(adjust-weights % biased-input error l-rate))]

        (when (zero? (rem i print-every))
          (println i " E:" error " I:" input " O:" output " P:" fired-act))

        (if (< i iterations)
          (recur (inc i) p' rest-sets)
          p')))))


#_
(defn adjust-perceptron [perceptron previous-layer-activations error]
  (p/check-input-compatbility perceptron previous-layer-activations)

  (let []
    (update perceptron :weights
            #(mapv (fn [w i]
                     (+ w ()))
                   % previous-layer-activations))))

#_
(defn adjust-perceptron [perceptron previous-layer-activations expected-activation]
  (p/check-input-compatbility perceptron previous-layer-activations)
  (let [{{ deriv-f :derivative} :activation-pair act :last-activation} perceptron
        error (* (- expected-activation act) (deriv-f act))]
    (update perceptron :weights
            #(mapv (fn [w i]
                     (+ w ()))
                   % previous-layer-activations))))


#_
(defn raw-adjust-by [input expected-output actual-output deriv-f]
  (let [error (- expected-output actual-output)]

    (nh/sum
      (fo))))
#_
(defn improve-weights [perceptron input-output-map learning-rate]
  (let [deriv-f (:derivative (:activation-pair perceptron))]
    (reduce
      (fn [acc-perc [ins out]]
        (let [])))))

