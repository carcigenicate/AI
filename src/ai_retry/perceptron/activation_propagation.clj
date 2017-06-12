(ns ai-retry.perceptron.activation-propagation
  (:require [ai-retry.perceptron.layer :as la]
            [ai-retry.perceptron.perceptron :as p]))

(defn fire-layer [layer input]
  (map #(p/fire % input) layer))

(defn fire-layers [layers input]
  (loop [[layer & rest-layers] layers
         activations input
         acc-layers []]

    (if layer
      (let [fired-layer (fire-layer layer activations)]
        (recur rest-layers
               (la/activations-of-layer fired-layer)
               (conj acc-layers fired-layer)))

      acc-layers)))