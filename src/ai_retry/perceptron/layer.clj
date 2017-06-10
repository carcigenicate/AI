(ns ai-retry.perceptron.layer
  (:require [ai-retry.perceptron.perceptron :as p]))

(defrecord Layer [perceptrons])

(defn new-layer [previous-layer-width layer-width perceptron-constructor]
  (vec
    (for [_ (range layer-width)]
      (perceptron-constructor previous-layer-width))))

(defn new-layers
  ([n-inputs n-layers layer-width perceptron-constructor]
   (new-layers n-inputs (repeat n-layers layer-width) perceptron-constructor))

  ([n-inputs layer-widths perceptron-constructor]
   (let []
     (loop [[width & widths] layer-widths
            last-width n-inputs
            acc []]
       (if width
         (recur widths
                width
                (conj acc (new-layer last-width width perceptron-constructor)))
         acc)))))

(defn fire-layer [layer input]
  (map #(p/fire % input) layer))

(defn fire-layers [layers input]
  (loop [[layer & rest-layers] layers
         activations input]
    (println "Activations:" activations)
    (if layer
      (recur rest-layers
             (fire-layer layer activations))

      activations)))