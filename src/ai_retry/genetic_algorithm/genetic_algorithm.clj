(ns ai-retry.genetic-algorithm.genetic-algorithm
  (:require [ai-retry.genetic-algorithm.population :as p]
            [ai-retry.genetic-algorithm.settings :as s]
            [helpers.general-helpers :as g])

  (:import [java.util Date])
  (:import [java.util Date]))

(defn date-stamp []
  (str (Date.)))

(defn print-population [pop]
  (doseq [g pop]
    (println g)))

; TODO: Write a version of judge-and-sort that doesnt automatically remove fitness information

(defn advance-generations [population settings max-generations rand-gen]
  (let [print-every (/ max-generations 200)
        problem-settings (:problem settings)
        ff (:fitness-f problem-settings)
        fit-err-comp (:fit-err-comp problem-settings)
        sorted-pop #(p/judge-and-sort-population % ff fit-err-comp)
        sample-fitness #(-> % first ff)
        perc #(g/prec-round 3 (* 100 (/ (double %) %2)))]

    (loop [gen 0
           acc-pop population]

      (if-not (>= gen max-generations)
        (let [advanced-pop (p/advance-population
                             acc-pop
                             settings
                             fit-err-comp
                             rand-gen)]

          (when (zero? (rem gen print-every))
            (let [sp (sorted-pop advanced-pop)]
              (println (str (date-stamp) " - " (count advanced-pop) "\n\t"
                            (perc gen max-generations) "% - " gen "\n\t"
                            "Best: "(sample-fitness sp)))

              (print-population (take 5 sp))
              (println "\n")))

          (recur (inc gen) advanced-pop))

        acc-pop))))
