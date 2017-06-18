(ns ai-retry.main
  (:require [ai-retry.helpers.number-helpers :as nh]
            [ai-retry.activation-functions :as af]

            [ai-retry.perceptron.activation-propagation :as ap]
            [ai-retry.perceptron.error-backpropagation :as eb]
            [ai-retry.perceptron.perceptron :as p]
            [ai-retry.perceptron.learning-sets :as ls]
            [ai-retry.perceptron.layer :as la]
            [ai-retry.perceptron.net :as n]
            [ai-retry.perceptron.test-net :as tn]

            [helpers.general-helpers :as g])
  (:gen-class))

(def error-learning-rate-multiple 5)

(def learning-rate 0.005)

(def error-epsilon 1e-10)

(defn average-abs-error [errors]
  (nh/average
    (map #(Math/abs ^double %) errors)))

(defn relative-error-symbol [old-error new-error]
  (cond
    (< old-error new-error) (char 8657) ; Up arrow
    (> old-error new-error) (char 8659) ; Down arrow
    :else \=))

(defn adjustment-loop [layers learning-set iterations]
  (let [sample-perc (ffirst layers)
        deriv (-> sample-perc :activation-pair :derivative)
        cycled-sets (cycle learning-set)
        print-every (/ iterations 50.0)]

    (loop [i 0
           acc-layers layers
           [[input output] & rest-sets] cycled-sets
           last-errors {}]

      (let [biased-input (p/biased-input (:biased? sample-perc) input)
            fired-layers (ap/fire-layers acc-layers biased-input)
            fired-acts (n/output-activations fired-layers)

            errors (eb/output-layer-errors output fired-acts deriv)
            av-error (average-abs-error errors)
            l-rate (* av-error error-learning-rate-multiple)
            layers' (eb/backprop-layers fired-layers biased-input errors l-rate)
            last-error (get last-errors input Double/MAX_VALUE)
            error-sym (relative-error-symbol last-error av-error)
            err-diff (Math/abs ^double (- last-error av-error))
            last-errors' (assoc last-errors input av-error)]

        (when (zero? (rem i print-every))
          #_
          (println i "E:" av-error "I/O:" input output "{" fired-acts
                   "}\n\tL:" l-rate "-" error-sym " " err-diff "\n")

          (println i error-sym)
          (clojure.pprint/pprint last-errors)
          (println))

        (if (< i iterations)
          (recur (inc i) layers' rest-sets last-errors')
          layers')))))

(defn pdf-test
  "Test scenario from a PDF titled: 3. The Back Propagation Algorithm
  CONFIRMED!"
  []
  (let [ls {[0.35 0.9] [0.5]}
        ap af/sigmoid
        der (:derivative ap)
        pc #(p/new-perceptron % (constantly 0) ap false)

        hl (la/new-layer 2 2 pc)
        ol (la/new-layer 2 1 pc)

        hl' (-> hl
                (assoc-in [0 :weights] [0.1 0.8])
                (assoc-in [1 :weights] [0.4 0.6]))
        ol' (assoc-in ol [0 :weights] [0.3 0.9])

        input (ffirst ls)

        net (conj [] hl' ol')
        fired-net (ap/fire-layers net input)
        acts (n/output-activations fired-net)
        errors (eb/output-layer-errors (last (first ls)) acts der)

        b-net (eb/backprop-layers fired-net input errors 1)]

    (comment
      (clojure.pprint/pprint fired-net)
      (println "Out Acts:" acts)
      (println "Errors:" errors)
      (clojure.pprint/pprint b-net))))


#_
(defn adjustment-loop [layer learning-set iterations]
  (let [sample-perc (first layer)
        deriv (-> sample-perc :activation-pair :derivative)
        cycled-sets (cycle learning-set)
        print-every (/ iterations 50.0)]

    (loop [i 0
           acc-layer layer
           [[input output] & rest-sets] cycled-sets]2

      (let [biased-input (p/biased-input (:biased? sample-perc) input)
            fired-layer (ap/fire-layer acc-layer biased-input)
            fired-acts (la/activations-of-layer fired-layer)

            errors (eb/output-layer-errors output fired-acts deriv)
            av-error (average-abs-error errors)
            l-rate (* av-error error-learning-rate-multiple)
            layer' (eb/backprop-layer fired-layer biased-input errors l-rate)]

        (when (zero? (rem i print-every))
          (println i " E:" av-error " I:" input " O:" output " P:" fired-acts
                   "\n\tL:" l-rate "\n"))

        (if (< i iterations)
          (recur (inc i) layer' rest-sets)
          layer')))))

(defn test-y [a b c]
  (double (/ (+ (* a b) c) 100)))

(def test-set
  {[1 2 3] [(test-y 1 2 3)]
   [9 8 7] [(test-y 9 8 7)]
   [3 6 9] [(test-y 3 6 9)]
   [7 6 5] [(test-y 7 6 5)]})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
