(ns ai-retry.perceptron.net
  (:require [ai-retry.perceptron.layer :as la]
            [helpers.general-helpers :as g]
            [helpers.save-manager :as sm]))

(def net-save-path "./saved-nets/")
(def save-extension "txt")

(def save-manager (sm/new-save-manager net-save-path save-extension))

(defn new-net [input-size output-size hidden-layer-width n-hidden-layers perceptron-constructor]
  (let [pre-output-width (if (zero? n-hidden-layers)
                           input-size
                           hidden-layer-width)
        hidden-layers (la/new-layers input-size
                                     n-hidden-layers
                                     hidden-layer-width
                                     perceptron-constructor)
        output-layer (la/new-layer pre-output-width
                                   output-size
                                   perceptron-constructor)]
    (conj hidden-layers output-layer)))

(defn output-activations [net]
  (la/activations-of-layer
    (last net)))

(defn save-net [net net-name]
  (sm/save save-manager net net-name))

(defn load-net [net-name]
  (sm/load save-manager net-name))
