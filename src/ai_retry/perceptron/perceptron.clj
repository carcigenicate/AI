(ns ai-retry.perceptron.perceptron
  (:require [ai-retry.helpers.number-helpers :as nh]))

(def bias -1)

(defrecord Perceptron [weights activation-pair biased?])

(defn new-perceptron [n-weights weight-f activation-pair biased?]
  (let [n-weights' (+ n-weights (if biased? 1 0))
        weights (vec
                  (for [_ (range n-weights')]
                    (weight-f)))]

    (->Perceptron weights activation-pair biased?)))

(defn- compatible-input? [perceptron input]
  (= (count input)
     (count (:weights perceptron))))

(defn- check-input-compatbility [perceptron input]
  (when-not (compatible-input? perceptron input)
    (throw (RuntimeException.
             (str "Invalid input (" input ") for perceptron (" perceptron ")")))))

(defn- weighted-biased-input [perceptron input]
  (let [{ws :weights b? :biased?} perceptron
        biased-input? (if b? (conj (vec input) bias) input)]
    (mapv * ws biased-input?)))

(defn fire [perceptron input]
  (let [processed-input (weighted-biased-input perceptron input)]
    (check-input-compatbility perceptron processed-input)

    (let [act-f (:activation (:activation-pair perceptron))
          weighted-sum (nh/sum processed-input)]

       (act-f weighted-sum))))