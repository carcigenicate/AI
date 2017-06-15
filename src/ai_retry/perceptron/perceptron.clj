(ns ai-retry.perceptron.perceptron
  (:require [ai-retry.helpers.number-helpers :as nh]
            [helpers.general-helpers :as g]))

(def bias -1)

(defrecord Perceptron [weights activation-pair biased? last-activation]
  Object
  (toString [self] (str (dissoc (into {} self) :activation-pair))))

(defn new-perceptron [n-weights weight-f activation-pair biased?]
  (let [n-weights' (+ n-weights (if biased? 1 0))
        weights (vec
                  (for [_ (range n-weights')]
                    (weight-f)))]

    (->Perceptron weights activation-pair biased? nil)))

(defn- compatible-input? [perceptron input]
  (= (count input)
     (count (:weights perceptron))))

(defn check-input-compatbility [perceptron input]
  (when-not (compatible-input? perceptron input)
    (throw (RuntimeException.
             (str "Invalid input (" input ") for perceptron (" perceptron ")")))))

(defn biased-input
  "If the perceptron is biased, a bias is added to the other activations."
  [biased? input]
  (if biased?
    (conj (vec input) bias)
    input))

(defn weighted-biased-input [perceptron input]
  (let [{ws :weights} perceptron
        input' (biased-input (:biased? perceptron) input)]
    (mapv * ws input')))

(defn fire [perceptron input]
  (let [processed-input (weighted-biased-input perceptron input)]
    (check-input-compatbility perceptron processed-input)

    (let [act-f (:activation (:activation-pair perceptron))
          weighted-sum (nh/sum processed-input)]

       (assoc perceptron :last-activation
              (act-f weighted-sum)))))

(defn random-weight [min-weight max-weight default-weight rand-gen]
  (let [rw (g/random-double min-weight max-weight rand-gen)]
    (if (zero? rw)
      default-weight
      rw)))
