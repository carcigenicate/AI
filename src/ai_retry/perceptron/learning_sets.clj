(ns ai-retry.perceptron.learning-sets
  (:require [helpers.general-helpers :as g]))

(defn input-size [learning-set]
  (count (ffirst learning-set)))

(defn output-size [learning-set]
  (count (second (first learning-set))))

(defn scale-set [min-in max-in min-out max-out learning-set]
  (let [f (fn [xs mi ma] (mapv #(g/map-range % mi ma 0 1) xs))]
    (into {}
      (for [[is os] learning-set]
        [(f is min-in max-in)
         (f os min-out max-out)]))))


(def and-set
  {[0 1] [0]
   [0 0] [0]
   [1 0] [0]
   [1 1] [1]})

(def nand-set
  {[0 1] [1]
   [0 0] [1]
   [1 0] [1]
   [1 1] [0]})

(def or-set
  {[0 0] [0]
   [0 1] [1]
   [1 0] [1]
   [1 1] [1]})

(def xor-set
  {[0 0] [0]
   [0 1] [1]
   [1 0] [1]
   [1 1] [0]})

(def not-set
  {[0] [1]
   [1] [0]})

(def identity-set
  {[1] [1]
   [0.5] [0.5]
   [0.13] [0.13]
   [0.123] [0.123]
   [0.4321] [0.4321]
   [0] [0]})

(def identity-triplet-set
  {[0.1 0.2 0.3] [0.1 0.2 0.3]
   [0.9 0.8 0.7] [0.9 0.8 0.7]
   [0.3 0.6 0.9] [0.3 0.6 0.9]
   [0.7 0.6 0.5] [0.7 0.6 0.5]
   [0.8 0.3 0.5] [0.8 0.3 0.5]})

(def
  parity-set
  "If the input is truthy, the first ouput will output 1, else the second output will output 1."
  {[0.2] [1 0]
   [0.3] [0 1]
   [0.5] [0 1]
   [0.6] [1 0]
   [0.8] [1 0]
   [0.7] [0 1]})