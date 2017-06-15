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
  previous-layer-error-map should be a map of {error weight-to-current-node, ...}"
  [activation derivative previous-layer-error-map]
  (* (derivative activation)
     (nh/sum
       (map (fn [[error weight]] (* error weight))
            previous-layer-error-map))))

(defn hidden-layer-errors [layer previous-layer-errors]
  (let [d #(-> % :activation-pair :derivative)
        m #(into {} (map vector previous-layer-errors (:weights %)))]

   (mapv #(hidden-node-error (:last-activation %) (d %) (m %))
         layer)))

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
        source-activations
        old-weights))
#_
(defn backprop-layer [layer next-layer-activations previous-layer-errors learning-rate]
  (mapv (fn [p e]
          (update p :weights
                    #(adjust-weights % next-layer-activations e learning-rate)))
        layer
        previous-layer-errors))

(defn backprop-layer [layer next-layer-activations layer-errors learning-rate]
  (mapv (fn [p e]
          (update p :weights
                  #(adjust-weights % next-layer-activations e learning-rate)))
        layer
        layer-errors))


(defn backprop-layers [layers input-activations output-errors learning-rate]
  (loop [[layer & [next-layer :as rest-layers]] (reverse layers)
         errors output-errors
         acc '()]

    (if layer
        (let [next-acts (if next-layer (la/activations-of-layer next-layer) input-activations)
              b-layer (backprop-layer layer next-acts errors learning-rate)
              next-errors (hidden-layer-errors next-layer errors)]

          (recur rest-layers next-errors
                 (conj acc b-layer)))

        (vec acc))))
