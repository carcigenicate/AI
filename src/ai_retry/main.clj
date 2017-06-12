(ns ai-retry.main
  (:require [ai-retry.helpers.number-helpers :as nh]
            [ai-retry.activation-functions :as af]

            [ai-retry.perceptron.activation-propagation :as ap]
            [ai-retry.perceptron.error-backpropagation :as eb]
            [ai-retry.perceptron.perceptron :as p]
            [ai-retry.perceptron.learning-sets :as ls]
            [ai-retry.perceptron.layer :as la]

            [helpers.general-helpers :as g])
  (:gen-class))

(def error-learning-rate-multiple 2)

(defn average-abs-error [errors]
  (nh/average
    (map #(Math/abs ^double %) errors)))

(defn adjustment-loop [layer learning-set iterations]
  (let [sample-perc (first layer)
        deriv (-> sample-perc :activation-pair :derivative)
        cycled-sets (cycle learning-set)
        print-every (/ iterations 50.0)]

    (loop [i 0
           acc-layer layer
           [[input output] & rest-sets] cycled-sets]

      (let [biased-input (p/biased-input (:biased? sample-perc) input)
            fired-layer (ap/fire-layer acc-layer biased-input)
            fired-acts (la/activations-of-layer fired-layer)

            errors (eb/output-layer-errors output fired-acts deriv)
            av-error (average-abs-error errors)
            l-rate (* av-error error-learning-rate-multiple)
            layer' (eb/backprop-layer fired-layer biased-input errors l-rate)]

        (when (zero? (rem i print-every))
          (println i " E:" av-error " I:" input " O:" output " P:" fired-acts))

        (if (< i iterations)
          (recur (inc i) layer' rest-sets)
          layer')))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
