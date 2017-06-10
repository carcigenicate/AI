(ns ai-retry.perceptron.learning
  (:require [ai-retry.perceptron.perceptron :as p]
            [ai-retry.helpers.number-helpers :as nh]))

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

