(ns ai-retry.perceptron.net
  (:require [ai-retry.perceptron.layer :as la]
            [helpers.general-helpers :as g]))

(defn new-net [input-size output-size hidden-layer-width n-hidden-layers perceptron-constructor]
  (let [hidden-layers (la/new-layers input-size
                                     n-hidden-layers
                                     hidden-layer-width
                                     perceptron-constructor)
        output-layer (la/new-layer hidden-layer-width
                                   output-size
                                   perceptron-constructor)]
    (conj hidden-layers output-layer)))

(defn output-activations [net]
  (la/activations-of-layer
    (last net)))