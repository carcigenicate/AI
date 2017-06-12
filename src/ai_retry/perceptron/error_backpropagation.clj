(ns ai-retry.perceptron.error-backpropagation
  (:require [ai-retry.perceptron.perceptron :as p]
            [ai-retry.perceptron.layer :as la]
            [ai-retry.helpers.number-helpers :as nh]
            [ai-retry.activation-functions :as af]
            [ai-retry.perceptron.learning-sets :as ls]

            [helpers.general-helpers :as g]))

(defn output-node-error [expected-output actual-output derivative]
  (* (derivative actual-output)
     (- expected-output actual-output)))

(defn output-layer-errors [expected-outputs actual-outputs derivative]
  ; TODO?: Check if the list lengths are correct? Will truncate the shorter list if not.
  (mapv #(output-node-error % %2 derivative)
        expected-outputs actual-outputs))

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
  (mapv (fn [a w]
          (adjust-weight w a target-error learning-rate))
        source-activations old-weights))

(defn backprop-layer [layer next-layer-activations previous-layer-errors learning-rate]
  (mapv (fn [p e]
          (update p :weights
                    #(adjust-weights % next-layer-activations e learning-rate)))
        layer previous-layer-errors))


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

