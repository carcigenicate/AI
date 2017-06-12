(ns ai-retry.perceptron.learning-sets)

(defn input-size [learning-set]
  (count (ffirst learning-set)))

(defn output-size [learning-set]
  (count (second (first learning-set))))

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